
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString.Char8 as S
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString (recv, send)
import Data.ByteString (ByteString)
import Data.List (isPrefixOf)

maxConn :: Int
maxConn = 1024

recvBufferSize :: Int
recvBufferSize = 1024


port :: String
port = "3000"


main :: IO ()
main = runTCPServer Nothing port serve


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
        forkFinally (handleConn conn) (const $ gracefulClose conn 5000) -- 5 second timeout to wait for lagging messages

serve :: Socket -> IO ()
serve socket = do
        msg <- recv socket recvBufferSize
        unless (S.null msg) $ do -- if message is null the client has disconnected
          let msgStr = S.unpack msg
          putStrLn msgStr             
          let response = handleMsg msgStr
          putStrLn response             
          send socket $ S.pack $ response
          serve socket
        where
          handleMsg msg   
              | "GET" `isPrefixOf` msg = "GET request"
              | "POST" `isPrefixOf` msg ="POST request"
              | "OPTIONS" `isPrefixOf` msg =
                  "HTTP/1.1 200 OK\r\n\
                  \Access-Control-Allow-Origin: http://127.0.0.1:8231\r\n\
                  \Access-Control-Allow-Methods: POST, GET, OPTIONS\r\n\
                  \Access-Control-Allow-Headers: Content-Type\r\n\r\n"
              | otherwise                   = "Unknown request"

