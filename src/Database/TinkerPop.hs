module Database.TinkerPop where
import Database.TinkerPop.Types

import Prelude hiding (putStrLn)
import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack, append)
import qualified Data.Text as T
import Data.Text.IO
import Data.Text.Encoding
import Data.Aeson (encode, eitherDecodeStrict, Object, Array, Value)
import Data.Aeson.TH
import Data.Aeson.QQ
import Control.Concurrent
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.STM as S
import qualified Control.Concurrent.STM.TChan as S
import qualified Control.Concurrent.STM.TVar as S
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Control.Lens

data ResponseStatus = ResponseStatus {
    _responseStatusMessage :: Text
    , _responseStatusCode :: Int
    , _responseStatusAttributes :: Object
} deriving (Show)
makeFields ''ResponseStatus
$(deriveJSON defaultOptions {fieldLabelModifier = lowerFirst.(drop 15)} ''ResponseStatus)

data ResponseResult = ResponseResult {
    _responseResultData' :: Maybe [Value]
    , _responseResultMeta :: Object
} deriving (Show)
makeFields ''ResponseResult
$(deriveJSON defaultOptions  {fieldLabelModifier = \x -> if x == "_responseResultData'" then "data" else lowerFirst (drop 15 x)} ''ResponseResult)

data ResponseMessage = ResponseMessage {
    _responseMessageRequestId :: Text
    , _responseMessageStatus :: ResponseStatus
    , _responseMessageResult :: ResponseResult
} deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = lowerFirst.(drop 16)} ''ResponseMessage)
makeFields ''ResponseMessage

data RequestArgs = RequestArgs {
    _requestArgsGremlin :: Text
    , _requestArgsBindings :: Value
    , _requestArgsLanguage :: Text
    , _requestArgsBatchSize :: Int
}
$(deriveJSON defaultOptions{fieldLabelModifier = lowerFirst.(drop 12)} ''RequestArgs)
makeFields ''RequestArgs

data RequestMessage = RequestMessage {
   _requestMessageRequestId :: Text
   , _requestMessageOp :: Text
   , _requestMessageProcessor :: Text
   , _requestMessageArgs :: RequestArgs
}
$(deriveJSON defaultOptions{fieldLabelModifier = lowerFirst.(drop 15)} ''RequestMessage)
makeFields ''RequestMessage

data Connection = Connection {
    ws :: WS.Connection
    , chans :: S.TVar (M.Map Text (S.TChan (Either String ResponseMessage)))
}

run :: String -> Int -> (Connection -> IO ()) -> IO ()
run host port app = do    
    WS.runClient host port "/" $ \ws -> do
        chans <- S.newTVarIO M.empty
        let conn = Connection ws chans
        handle conn
        app conn
        close conn

handle conn = do
    forkIO $ forever $ do
        msg <- WS.receiveData (ws conn) :: IO Text
--        putStrLn $ "recv: " `append` msg
        case eitherDecodeStrict (encodeUtf8 msg) of
         Right r -> do
             cs <- S.readTVarIO (chans conn)
             case M.lookup (r ^. requestId) cs of
              Just chan -> S.atomically $ S.writeTChan chan (Right r)
              Nothing -> putStrLn $ "ERROR: chan not found"
         Left s -> putStrLn $ "ERROR: parse response message: " `append` (pack s)

close conn = do
--    putStrLn "will close"
    WS.sendClose (ws conn) ("Bye!" :: Text)

submit :: Connection -> Text -> IO (Either String [Value])
submit conn gremlin = do
    req <- buildRequest gremlin
    chan <- S.newTChanIO
    S.atomically $ S.modifyTVar (chans conn) $ M.insert (req ^. requestId) chan
    WS.sendTextData (ws conn) (encode req)
    recv chan []
  where
    recv chan xs = do
        eres <- S.atomically $ S.readTChan chan
        case eres of
         Right r
             | elem statusCode [200, 206] -> do                   
                   let Just d = (r ^. result ^. data')
                       xs' = xs ++ d
                   if statusCode == 206 then recv chan xs' else return $ Right xs'
             | otherwise -> return $ Left (unpack $ r ^. status ^. message)
           where statusCode = (r ^. status ^. code)
         Left x -> return $ Left x

buildRequest gremlin = do
    uuid <- U.toText <$> U.nextRandom
    return $ RequestMessage uuid "eval" "" $
        RequestArgs gremlin [aesonQQ| {"x": 1}|] "gremlin-groovy" 2


