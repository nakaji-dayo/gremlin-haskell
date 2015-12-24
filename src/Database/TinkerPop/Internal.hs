module Database.TinkerPop.Internal where
import Database.TinkerPop.Types

import Prelude hiding (putStrLn)
import Data.Text (Text, pack, append)
import Data.Text.IO
import Data.Text.Encoding
import qualified Data.Map.Strict as M
import qualified Control.Monad.STM as S
import qualified Control.Concurrent.STM.TChan as S
import qualified Control.Concurrent.STM.TVar as S
import Control.Concurrent
import Control.Monad (forever)
import Control.Lens
import Data.Aeson (eitherDecodeStrict)
import qualified Network.WebSockets as WS

-- | check HTTP status code is success
inStatus2xx :: Int -> Bool
inStatus2xx x =  (x `quot` 100) == 2

-- | Start thread to recieve response
handle :: Connection -> IO (ThreadId)
handle conn = do
    forkIO $ forever $ do
        msg <- WS.receiveData (conn ^. socket) :: IO Text
--        putStrLn $ "recv: " `append` msg
        case eitherDecodeStrict (encodeUtf8 msg) of
         Right r -> do
             cs <- S.readTVarIO (conn ^. chans)
             case M.lookup (r ^. requestId) cs of
              Just chan -> S.atomically $ S.writeTChan chan (Right r)
              Nothing -> putStrLn $ "ERROR: chan not found"
         Left s -> putStrLn $ "ERROR: parse response message: " `append` (pack s)

-- | Close connection
close :: Connection -> IO ()
close conn = do
--    putStrLn "will close"
    WS.sendClose (conn ^. socket) ("Bye!" :: Text)
