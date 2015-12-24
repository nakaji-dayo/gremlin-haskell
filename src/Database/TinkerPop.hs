module Database.TinkerPop where

import Database.TinkerPop.Types
import Database.TinkerPop.Internal

import Prelude hiding (putStrLn)
import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack, append)
-- import qualified Data.Text as T
import Data.Text.IO
import Data.Text.Encoding
import Data.Aeson (encode, eitherDecodeStrict, Value)
import Data.Aeson.QQ
import Control.Concurrent
import Control.Monad (forever)
-- import Control.Monad.Trans (liftIO)
import qualified Control.Monad.STM as S
import qualified Control.Concurrent.STM.TChan as S
import qualified Control.Concurrent.STM.TVar as S
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Control.Lens

run :: String -> Int -> (Connection -> IO ()) -> IO ()
run host port app = do    
    WS.runClient host port "/" $ \ws -> do
        cs <- S.newTVarIO M.empty
        let conn = Connection ws cs
        _ <- handle conn
        app conn
        close conn

handle :: Connection -> IO (ThreadId)
handle conn = do
    forkIO $ forever $ do
        msg <- WS.receiveData (conn ^. socket) :: IO Text
        putStrLn $ "recv: " `append` msg
        case eitherDecodeStrict (encodeUtf8 msg) of
         Right r -> do
             cs <- S.readTVarIO (conn ^. chans)
             case M.lookup (r ^. requestId) cs of
              Just chan -> S.atomically $ S.writeTChan chan (Right r)
              Nothing -> putStrLn $ "ERROR: chan not found"
         Left s -> putStrLn $ "ERROR: parse response message: " `append` (pack s)

close :: Connection -> IO ()
close conn = do
--    putStrLn "will close"
    WS.sendClose (conn ^. socket) ("Bye!" :: Text)

submit :: Connection -> Gremlin -> Maybe Binding -> IO (Either String [Value])
submit conn body binding = do
    req <- buildRequest body binding
    chan <- S.newTChanIO
    S.atomically $ S.modifyTVar (conn ^. chans) $ M.insert (req ^. requestId) chan
    WS.sendTextData (conn ^. socket) (encode req)
    recv chan []
  where
    recv chan xs = do
        eres <- S.atomically $ S.readTChan chan
        case eres of
         Right r
             | inStatus2xx statusCode -> do                   
                   let xs' = case (r ^. result ^. data') of
                           Just d -> xs ++ d
                           Nothing -> xs
                   if statusCode == 206 then recv chan xs' else return $ Right xs'
             | otherwise -> return $ Left (unpack $ r ^. status ^. message)
           where statusCode = (r ^. status ^. code)
         Left x -> return $ Left x

buildRequest :: Gremlin -> Maybe Binding -> IO RequestMessage
buildRequest body binding = do
    uuid <- U.toText <$> U.nextRandom
    return $ RequestMessage uuid "eval" "" $
        RequestArgs body binding "gremlin-groovy" 2
