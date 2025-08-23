
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Data.List (isPrefixOf)
import Text.Parsec (parse)
import Data.List.Split (splitOn)
import qualified Data.String as DS
import Control.Exception (try, SomeException)
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple (Only(..))
import Data.Time
import System.Environment (getArgs)
import Control.Monad
import System.Random

maxConn :: Int
maxConn = 1024

recvBufferSize :: Int
recvBufferSize = 1024

gracefulCloseTimeout :: Int
gracefulCloseTimeout = 5000

port :: String
port = "8231"



main :: IO ()
main = do
  args <- getArgs
  case args of
      [] -> putStrLn "Usage: program <gen|serve>"
      ("serve":_) -> do
          conn <- SQL.open "penger.db"
          alive <- checkDBAlive conn
          if alive
            then do
              putStrLn "Connected to database"
              hasActivationTable <- ensureActivationTable conn
              if hasActivationTable then do
                runTCPServer Nothing port (serve conn)
                SQL.close conn
              else do
                putStrLn "Failed to create activation code table in database"
                SQL.close conn
            else putStrLn "Failed to connect to database"

      ("gen":_)  -> do
          conn <- SQL.open "penger.db"
          alive <- checkDBAlive conn
          if alive
            then do
              putStrLn "Connected to database"
              hasActivationTable <- ensureActivationTable conn
              if hasActivationTable then do
                createActivationCode conn 7
                SQL.close conn
              else do
                putStrLn "Failed to create activation code table in database"
                SQL.close conn
            else putStrLn "Failed to connect to database"

      _ -> putStrLn "Unknown command"





checkDBAlive :: SQL.Connection -> IO Bool
checkDBAlive conn = do
    result <- try $ SQL.query_ conn (DS.fromString "SELECT 1") :: IO (Either SomeException [Only Int])
    case result of
        Left  _ -> return False
        Right _ -> return True


ensureActivationTable :: SQL.Connection -> IO Bool
ensureActivationTable conn = do
    result <- try $ SQL.execute_ conn (DS.fromString "CREATE TABLE IF NOT EXISTS activation_codes (code TEXT PRIMARY KEY, expires_on DATE)") :: IO (Either SomeException ())
    case result of
      Left err -> do
        putStrLn $ "Failed to ensure activation_codes table: " ++ show err
        return False
      Right _ -> do
        putStrLn "activation_codes table exists"
        return True

createActivationCode :: SQL.Connection -> Int -> IO ()
createActivationCode conn expires_in = do
                                    activationCode <- genUniqueCode 16
                                    now <- getCurrentTime
                                    let expires_on = addUTCTime (fromIntegral $ expires_in * 86400) now -- 86400 = seconds in 1 day

                                    result <- addCodeToDb conn activationCode expires_on
                                    case result of
                                      Left err -> do
                                        putStrLn $ "DB error: " ++ show err
                                        return ()

                                      Right _ -> do
                                        putStrLn $ "Created activation code \"" ++ show activationCode ++ "\" which expires on " ++ show expires_on ++ " (" ++  show expires_in ++ ") days"
                                        return ()

                                  where
                                    randomString n = replicateM n randomChar
                                      where
                                        chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
                                        randomChar = do
                                          i <- randomRIO (0, length chars - 1)
                                          return (chars !! i)

                                    genUniqueCode n = do
                                      activationCode <- randomString 16

                                      result <- try $ SQL.query conn
                                        (DS.fromString "SELECT code FROM activation_codes WHERE code = (?)")
                                        (SQL.Only activationCode)
                                        :: IO (Either SomeException [(String, UTCTime)])

                                      case result of
                                        Left err -> do
                                          putStrLn $ "DB error: " ++ show err
                                          genUniqueCode n
                                        Right code
                                          | null code -> return activationCode
                                          | otherwise -> genUniqueCode n

                                    addCodeToDb conn activationCode expiry = do
                                                                  try $ SQL.execute conn
                                                                    (DS.fromString "INSERT INTO activation_codes (code, expires_on) VALUES (?, ?)")
                                                                    (activationCode, expiry) :: IO (Either SomeException ())


runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port handleConn = do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream }
        addrs <- getAddrInfo (Just hints) mhost (Just port)
        case addrs of
          []      -> error "No addresses found"
          (x:_)   -> return x

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock $ addrAddress addr
        listen sock maxConn
        return sock

    loop sock = forever $ E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> void $
        forkFinally (handleConn conn) (const $ gracefulClose conn gracefulCloseTimeout)

serve :: SQL.Connection -> Socket -> IO ()
serve conn socket = do
        msg <- recv socket recvBufferSize
        unless (S.null msg) $ do
          let msgStr = S.unpack msg
          response <- handleMsg conn msgStr
          putStrLn "[RESPONSE]"
          putStrLn response
          send socket (S.pack response)
          serve conn socket


handleMsg :: SQL.Connection -> String -> IO String
handleMsg conn msgStr =
        let msgLines = lines msgStr in
        case msgLines of
          [] -> return $ "HTTP/1.1 400 Bad Request\r\n"
                      ++ "\r\n"
                      ++ "Empty request"
          (firstLine:_) ->
            case words firstLine of
              method:path:_ ->
                case method of
                  "GET" -> case path of
                            "/" ->
                              do
                                putStrLn $ "[GET] \"" ++ path ++ "\""
                                page <- readFile "index.html"
                                return $ "HTTP/1.1 200 OK\r\n"
                                      ++ "Content-Length: " ++ show (length page) ++ "\r\n"
                                      ++ "Content-Type: text/html\r\n"
                                      ++ "\r\n"
                                      ++ page
                            _ ->
                              return $ "HTTP/1.1 404 Not Found\r\n"
                                    ++ "Content-Type: text/plain\r\n"
                                    ++ "Content-Length: 9\r\n"
                                    ++ "\r\n"
                                    ++ "Not Found"

                  "POST" ->
                    do
                      putStrLn $ "[POST] \"" ++ path ++ "\""
                      let body = init $ unlines $ drop 1 $ dropWhile (/= "\r")  msgLines
                      putStrLn $ "Body: " ++ body
                      case parseBody body of
                        Nothing ->
                          return $ "HTTP/1.1 400 Bad Request\r\n"
                                ++ "Content-Type: text/plain\r\n"
                                ++ "\r\n"
                                ++ "Malformed request"
                        Just (activation, username, password) -> do
                            status <- register conn activation username password
                            case status of
                                Succes ->
                                  return $ "HTTP/1.1 200 OK\r\n"
                                        ++ "Content-Type: text/plain\r\n"
                                        ++ "Content-Length: 15\r\n"
                                        ++ "\r\n"
                                        ++ "Registered user"
                                UserExists ->
                                  return $ "HTTP/1.1 403 Conflict\r\n"
                                        ++ "Content-Type: text/plain\r\n"
                                        ++ "Content-Length: 9\r\n"
                                        ++ "\r\n"
                                        ++ "User already exists"
                                ActivationWrong ->
                                  return $ "HTTP/1.1 400 Bad Request\r\n"
                                        ++ "Content-Type: text/plain\r\n"
                                        ++ "Content-Length: 21\r\n"
                                        ++ "\r\n"
                                        ++ "Wrong activation code"

                  "OPTIONS" ->
                      return $ "HTTP/1.1 200 OK\r\n"
                            ++ "Access-Control-Allow-Origin: http://127.0.0.1:8231\r\n"
                            ++ "Access-Control-Allow-Methods: POST, GET, OPTIONS\r\n"
                            ++ "Access-Control-Allow-Headers: Content-Type\r\n"
                            ++ "\r\n"
                  _ ->
                      return $ "HTTP/1.1 400 Bad Request\r\n"
                            ++ "Content-Type: text/plain\r\n"
                            ++ "\r\n"
                            ++ "Unknown request"
              [] ->
                return $ "HTTP/1.1 400 Bad Request\r\n"
                      ++ "Content-Type: text/plain\r\n"
                      ++ "\r\n"
                      ++ "Malformed request"

parseBody :: String -> Maybe (String, String, String)
parseBody [] = Nothing
parseBody body =
    let pairs = splitOn "," (drop 1 $ init body) in
    if length pairs < 3
       then Nothing
       else
           let activation = parsePair $ pairs !! 0
               username   = parsePair $ pairs !! 1
               password   = parsePair $ pairs !! 2
           in Just (activation, username, password)
            where
              parsePair pair = init $ drop 2 $ dropWhile (/=':') pair

data RegisterStatus = Succes | UserExists | ActivationWrong | ActivationExpired | DatabaseError
  deriving (Show, Eq, Enum)


register ::  SQL.Connection -> String -> String -> String -> IO RegisterStatus
register conn activation username password =
  do
    putStrLn $ "Registering user " ++ username ++ " with password " ++ password ++ " and activation code " ++ activation

    userRowsResult <- try $ SQL.query_ conn (DS.fromString "SELECT user_id, username FROM users") :: IO (Either SomeException [(Int, String)])

    case userRowsResult of
      Left err -> do
          putStrLn $ "DB error: " ++ show err
          return DatabaseError

      Right userRows ->
        if username `elem` map snd userRows
          then do
            putStrLn $ "User " ++ username ++ " already exists"
            return UserExists
          else do
            activationRowsResult <- try $ SQL.query conn
              (DS.fromString "SELECT code, expires_on FROM activation_codes WHERE code = ?")
              (SQL.Only activation) :: IO (Either SomeException [(String, UTCTime)])

            case activationRowsResult of
              Left err -> do
                putStrLn $ "DB error: " ++ show err
                return DatabaseError

              Right activationRows ->
                case activationRows of
                  [] -> do
                      putStrLn $ "Activation code \"" ++ activation ++ "\" not found"
                      return ActivationWrong
                  (_, expires):_ -> do
                      now <- getCurrentTime
                      if expires < now
                        then do
                          putStrLn $ "Activation code \"" ++ activation ++ "\" expired"
                          return ActivationExpired
                        else do
                          putStrLn $ "Activation code \"" ++ activation ++ "\" is valid, creating user..."
                          result <- try $ SQL.execute conn
                            (DS.fromString "INSERT INTO users (username, password) VALUES (?, ?)")
                            (username, password) :: IO (Either SomeException ())
                          case result of
                            Left err -> do
                              putStrLn $ "DB error: " ++ show err
                              return DatabaseError
                            Right _ -> do
                              putStrLn "Registered user"
                              return Succes





