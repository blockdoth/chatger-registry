
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Char8 as BS8
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
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist)


import qualified Crypto.Hash as Hash
import qualified Crypto.Random as CR
import qualified Data.ByteArray as BA
import Text.XHtml (password)


maxConn :: Int
maxConn = 1024

recvBufferSize :: Int
recvBufferSize = 1024

gracefulCloseTimeout :: Int
gracefulCloseTimeout = 5000

port :: String
port = "8231"

databaseDefaultPath :: String
databaseDefaultPath = "./penger.db"


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("serve":rest) -> withDB (getDbPath rest) $ \conn -> runTCPServer Nothing port (serve conn)
    ("gen":rest)   -> withDB (getDbPath rest) $ \conn -> genActivationCode conn 7
    _              -> usage
  where
    usage = putStrLn "Incorrect command, usage: regger <gen|serve> [-db <path>]"
    getDbPath ("-db":path:_) = path
    getDbPath _              = databaseDefaultPath

withDB :: String -> (SQL.Connection -> IO ()) -> IO ()
withDB dbPath action = do
  exists <- doesFileExist dbPath

  if not exists
    then putStrLn $ "Database not found at " ++ dbPath
    else do
      conn  <- SQL.open dbPath
      alive <- checkDBAlive conn

      if not alive
        then putStrLn "Failed to connect to database"
        else do
          putStrLn $ "Connected to database at " ++ dbPath
          ok <- ensureDnStructure conn
          if ok then action conn else putStrLn "Failed to create activation code table in database"
      SQL.close conn

  where
    checkDBAlive conn = do
      result <- try $ SQL.query_ conn (DS.fromString "SELECT 1") :: IO (Either SomeException [Only Int])
      case result of
          Left  _ -> return False
          Right _ -> return True

    ensureDnStructure conn = do
      result <- try $ SQL.execute_ conn (DS.fromString "CREATE TABLE IF NOT EXISTS activation_codes (code TEXT PRIMARY KEY, expires_on DATE, used BOOL)") :: IO (Either SomeException ())
      case result of
        Left err -> do
          putStrLn $ "Failed to ensure activation_codes table: " ++ show err
          return False
        Right _ -> do
          return True

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer hostname port handleConn = do
    putStrLn $ "Started listening on port " ++ port
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      addrs <- getAddrInfo (Just hints) hostname (Just port)
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
  unless (BS8.null msg) $ do
    let request = BS8.unpack msg
    response <- handleRequest conn request
    send socket (BS8.pack response)
    serve conn socket


handleRequest :: SQL.Connection -> String -> IO String
handleRequest conn request =
  case lines request of
    [] ->  return (badRequest400 "Empty request")
    (firstLine:requestLines) -> case words firstLine of
      (method:path:_) -> handleRoute_ conn method path requestLines
      _ -> return (badRequest400 "Malformed request")

data RegisterStatus = RegisterSucces | UserAlreadyExists | ActivationWrong | ActivationExpired | ActivationAlreadyUsed | DatabaseError
  deriving (Show, Eq, Enum)


handleRoute_ :: SQL.Connection -> String -> String -> [String] -> IO String
handleRoute_ conn method path rest = do
  putStrLn $ "Handling route [" ++ method ++ "] " ++ show path
  handleRoute conn method path rest


handleRoute :: SQL.Connection -> String -> String -> [String] -> IO String
handleRoute _ "GET" "/" _ = do
  page <- readFile "index.html"
  return $ ok200HTML page

-- handleRoute conn "GET" _ _ = do
--     return (notFound404 "Not Found")

handleRoute _ "OPTIONS" _ _ = do
  return ok200CORS

handleRoute conn "POST" "/register" lines = do
  case parseBody lines of
    Nothing -> return (badRequest400 "Malformed request")
    Just (activation, username, password) -> do
      -- putStrLn $ "\tRegistering user " ++ show username
      status <- runExceptT $ register conn activation username password
      return $ case status of
        Left err -> case err of
          UserAlreadyExists      -> conflict409 "User already exists"
          ActivationWrong -> badRequest400 "Wrong activation code"
          ActivationExpired -> badRequest400 "Activation code expired"
          ActivationAlreadyUsed -> badRequest400 "Activation code already used"
          DatabaseError   -> serverError500 "Database error"
        Right RegisterSucces -> ok200 "Registered user"
  where
    extractBodyLines lines = init $ unlines $ drop 1 $ dropWhile (/= "\r") lines
    parseBody lines = parseBodyJSon $ extractBodyLines lines
    parseBodyJSon [] = Nothing
    parseBodyJSon body =
        case map parsePair $ splitOn "," $ dropBrackets body of
          [activation, username, password] -> Just (activation, username, password)
          _ -> Nothing
        where
          parsePair pair = init $ drop 2 $ dropWhile (/=':') pair
          dropBrackets str = drop 1 $ init str

handleRoute _ "POST" _ _ = do
  return (notFound404 "Not Found")


safeQuery :: IO a -> IO (Either RegisterStatus a)
safeQuery action = do
  res <- try action
  case res of
    Left (err :: SomeException) -> do
      putStrLn $ "\tDB error: " ++ show err
      return (Left DatabaseError)
    Right val -> return (Right val)

safeQueryT :: IO a -> ExceptT RegisterStatus IO a
safeQueryT action = ExceptT $ safeQuery action

register :: SQL.Connection -> String -> String -> String -> ExceptT RegisterStatus IO RegisterStatus
register conn activation username password = do

  userRows <- safeQueryT $
    SQL.query_ conn (DS.fromString "SELECT user_id, username FROM users")
    :: ExceptT RegisterStatus IO [(Int, String)]

  when (username `elem` map snd userRows) $ do
    liftIO $ putStrLn "\tUser already exists"
    throwE UserAlreadyExists

  activationRows <- safeQueryT $
    SQL.query conn
      (DS.fromString "SELECT code, expires_on, used FROM activation_codes WHERE code = ?")
      (SQL.Only activation)
    :: ExceptT RegisterStatus IO [(String, UTCTime, Bool)]

  case activationRows of
    [] -> do
      liftIO $ putStrLn "\tActivation code not found"
      throwE ActivationWrong
    (_, expires, used):_ -> do
      now <- liftIO getCurrentTime
      when used $ do
        liftIO $ putStrLn "\tActivation code already used"
        throwE ActivationAlreadyUsed

      when (expires < now) $ do
        liftIO $ putStrLn "\tActivation code expired"
        throwE ActivationExpired

  (passwordHashed, salt) <- liftIO $ hashAndSaltPassword password
  _ <- safeQueryT $
    SQL.execute conn
      (DS.fromString "INSERT INTO users (username, password_hash, salt) VALUES (?, ?, ?)")
      (username, passwordHashed, salt)

  _ <- safeQueryT $
    SQL.execute conn
      (DS.fromString "UPDATE activation_codes SET  used = true WHERE code = ?")
      (SQL.Only activation)

  liftIO $ putStrLn $ "\tRegistered user" ++ username

  return RegisterSucces


hashAndSaltPassword :: String -> IO (BS.ByteString, BS.ByteString)
hashAndSaltPassword password = do
  salt <- CR.getRandomBytes 32
  let hashed = BA.convert (Hash.hash (salt <> BS8.pack password) :: Hash.Digest Hash.SHA3_256)
  return (hashed, salt)


httpResponse :: String -> [(String, String)] -> String -> String
httpResponse status headers body =
    "HTTP/1.1 " ++ status ++ "\r\n"
    ++ concatMap (\(k,v) -> k ++ ": " ++ v ++ "\r\n") headers
    ++ "\r\n"
    ++ body


ok200 :: String -> String
ok200 body = httpResponse "200 OK"
  [
    ("Content-Type", "text/plain"),
    ("Content-Length", show (length body))
  ] body

ok200HTML :: String -> String
ok200HTML body = httpResponse "200 OK"
  [
    ("Content-Type", "text/html"),
    ("Content-Length", show (length body))
  ] body

ok200CORS :: String
ok200CORS = httpResponse "200 OK"
  [
    ("Access-Control-Allow-Origin", "http://127.0.0.1:8231"),
    ("Access-Control-Allow-Methods", "POST, GET, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type")
  ] ""

badRequest400 :: String -> String
badRequest400 = httpResponse "400 Bad Request"
  [
    ("Content-Type", "text/plain")
  ]

notFound404 :: String -> String
notFound404 body = httpResponse "404 Not Found"
  [
    ("Content-Type", "text/plain") ,
    ("Content-Length", show (length body))
  ] body

conflict409 :: String -> String
conflict409 body = httpResponse "409 Conflict"
  [
    ("Content-Type", "text/plain") ,
    ("Content-Length", show (length body))
  ] body

serverError500 :: String -> String
serverError500 body = httpResponse "500 Server Error"
  [
    ("Content-Type", "text/plain") ,
    ("Content-Length", show (length body))
  ] body

genActivationCode :: SQL.Connection -> Int -> IO ()
genActivationCode conn expires_in = do
  activationCode <- genUniqueCode 16
  now <- getCurrentTime
  let expires_on = addUTCTime (fromIntegral $ expires_in * 86400) now -- 86400 = seconds in 1 day

  result <- addCodeToDb conn activationCode expires_on
  case result of
    Left err -> putStrLn $ "DB error: " ++ show err
    Right _ -> putStrLn $ "Created activation code " ++ show activationCode ++ " which expires on " ++ show expires_on ++ " (" ++  show expires_in ++ ") days"


  where
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    maxRetries = 5

    randomString n = replicateM n $ do
      i <- randomRIO (0, length chars - 1)
      return (chars !! i)

    genUniqueCode n = genUniqueCode_ n 1
    genUniqueCode_ n tries
      | tries > maxRetries = error "Failed to generate unique activation code after max retries"
      | otherwise = do
        code <- randomString n
        existing <- try (SQL.query conn
                           (DS.fromString "SELECT code FROM activation_codes WHERE code = ?")
                           (SQL.Only code)) :: IO (Either SomeException [SQL.Only String])
        case existing of
          Left err -> do
            putStrLn $ "DB error: " ++ show err
            genUniqueCode_ n (tries + 1)
          Right rows
            | null rows -> return code
            | otherwise -> genUniqueCode_ n (tries + 1)

    addCodeToDb conn activationCode expiry = do
      try $ SQL.execute conn
        (DS.fromString "INSERT INTO activation_codes (code, expires_on, used) VALUES (?, ?, false)")
        (activationCode, expiry) :: IO (Either SomeException ())

