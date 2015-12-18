module Database.TinkerPop where

import Prelude hiding (putStrLn)
import qualified Network.WebSockets as WS
import Data.Char (toLower)
import Data.Text (Text, pack, append)
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

data Connection = Connection {
    ws :: WS.Connection
    , chan :: S.TChan Text
}

data ResponseStatus = ResponseStatus {
    message :: Text
    , code :: Int
    , attributes :: Object
} deriving (Show)
$(deriveJSON defaultOptions ''ResponseStatus)

data ResponseResult = ResponseResult {
    resultData :: [Value]
    , resultMeta :: Object
} deriving (Show)
$(deriveJSON defaultOptions {fieldLabelModifier = (map toLower).(drop 6)} ''ResponseResult)

data ResponseMessage = ResponseMessage {
    requestId :: Text
    , status :: ResponseStatus
    , result :: ResponseResult
} deriving (Show)
$(deriveJSON defaultOptions ''ResponseMessage)

run :: String -> Int -> (Connection -> IO ()) -> IO ()
run host port app = do    
    WS.runClient host port "/" $ \ws -> do
        chan <- S.newTChanIO
        let conn = Connection ws chan
        handle conn
        app conn
        close conn

handle conn = do
    forkIO $ forever $ do
        msg <- WS.receiveData (ws conn) :: IO Text
        S.atomically $ S.writeTChan (chan conn) msg

close conn = do
    WS.sendClose (ws conn) ("Bye!" :: Text)

-- send :: Connection -> Value -> IO ([Object])
send conn v = do
    WS.sendTextData (ws conn) $ encode v
    recv []
  where
    recv xs = do
        msg <- S.atomically $ S.readTChan (chan conn)
        case eitherDecodeStrict (encodeUtf8 msg) of
         Right r -> do
             putStrLn "test"
             let xs' = xs ++ (resultData $ result r)
             if (code $ status r) == 206 then recv xs' else return $ Right xs'
         Left x -> return $ Left x
