module Database.TinkerPop.Internal where
import Database.TinkerPop.Types

import Prelude hiding (putStrLn)
import Data.Text (Text, pack, append)
import Data.Text.IO
import Data.Text.Encoding
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Control.Monad.STM as S
import qualified Control.Concurrent.STM.TChan as S
import qualified Control.Concurrent.STM.TVar as S
import Control.Concurrent
import Control.Monad
import Control.Lens
import Data.Aeson (eitherDecodeStrict)
import qualified Network.WebSockets as WS

-- | check HTTP status code is success
inStatus2xx :: Int -> Bool
inStatus2xx x =  (x `quot` 100) == 2

-- | Start thread to recieve response
handler :: Connection -> MVar () -> IO ()
handler conn done = do
    void $ forkIO $ do
        handle (wsExceptionHandler "child thread") $ forever $ do
            msg <- (WS.receiveData (conn ^. socket) :: IO Text)
            case eitherDecodeStrict (encodeUtf8 msg) of
             Right r -> do
                 cs <- S.readTVarIO (conn ^. chans)
                 case M.lookup (r ^. requestId) cs of
                  Just chan -> S.atomically $ S.writeTChan chan (Right r)
                  Nothing -> putStrLn $ "ERROR: chan not found"
             Left s -> putStrLn $ "ERROR: parse response message: " `append` (pack s)
        putMVar done ()

-- | Handle exception from websocket.
--
-- Ignore ConnectionClised exception that expected.
wsExceptionHandler :: Text -> SomeException -> IO ()
wsExceptionHandler label e = do
    -- putStrLn $ "handle exception[" `append` label `append` "]: " `append` (pack $ show e)
    case fromException e of
     Just WS.ConnectionClosed -> return ()
     _ -> do
         putStrLn $ "unexpect exception[" `append` label `append` "]: " `append` (pack $ show e)
         throw e

-- | Close connection
close :: Connection -> IO ()
close conn = do
--    putStrLn "will close"
    WS.sendClose (conn ^. socket) ("Bye!" :: Text)
