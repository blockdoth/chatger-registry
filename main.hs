
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (ByteString)

maxConn :: Int
maxConn = 1024

main :: IO ()
main = runTCPServer Nothing "3000" serve


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
        msg <- recv socket maxConn
        unless (S.null msg) $ do -- if message is null the client has disconnected
          handleMsg socket msg
          serve socket
        where
          handleMsg = sendAll 
