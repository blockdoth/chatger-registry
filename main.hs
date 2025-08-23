
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
  conn <- SQL.open "penger.db"
  alive <- checkDBAlive conn
  if alive
    then do 
      putStrLn "Connected to database"
      runTCPServer Nothing port (serve conn)
      SQL.close conn
    else putStrLn "Failed to connect to database"

checkDBAlive :: SQL.Connection -> IO Bool
checkDBAlive conn = do
    result <- try $ SQL.query_ conn (DS.fromString "SELECT 1") :: IO (Either SomeException [Only Int])
    case result of
        Left _  -> return False
        Right _ -> return True

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

data RegisterStatus = Succes | UserExists | ActivationWrong
  deriving (Show, Eq, Enum)


register ::  SQL.Connection -> String -> String -> String -> IO RegisterStatus
register conn activation username password = do
    putStrLn $ "Registering user " ++ username ++ " with password " ++ password ++ " and activation code " ++ activation
    
    userRows <- SQL.query_ conn (DS.fromString "SELECT user_id, username FROM users") :: IO [(Int, String)]
    let users = map snd userRows

    if username `elem` users 
      then do
        putStrLn $ "User " ++ username ++ " already exists"
        return UserExists
      else

        return Succes
