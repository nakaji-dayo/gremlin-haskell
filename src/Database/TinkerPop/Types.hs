-- |
-- Module: Database.TinkerPop.Types
-- Copyright: (c) 2015 The gremlin-haskell Authors
-- License     : BSD3
-- Maintainer  : nakaji.dayo@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
module Database.TinkerPop.Types where

import Data.Text hiding (drop, toLower)
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Aeson (Object, Value(..), FromJSON(..), (.:))
import Data.Aeson.TH
import Control.Lens
import Control.Monad (mzero)

import qualified Network.WebSockets as WS
import qualified Control.Concurrent.STM.TChan as S
import qualified Control.Concurrent.STM.TVar as S

import Database.TinkerPop.Internal.GraphSON (GraphSON(..))

-- | Represent Gremlin code
type Gremlin = Text

-- | A Map of key/value pairs
type Binding = Object

-- | parameters to pass to Gremlin Server. (TODO: The requirements for the contents of this Map are dependent on the op selected.)
data RequestArgs = RequestArgs {
    _requestArgsGremlin :: Text
    -- ^ The Gremlin script to evaluate    
    , _requestArgsBindings :: Maybe Binding
      -- ^ A map of key/value pairs to apply as variables in the context of the Gremlin script
    , _requestArgsLanguage :: Text
      -- ^ The flavor used (e.g. gremlin-groovy)
    , _requestArgsBatchSize :: Maybe Int
      -- ^ When the result is an iterator this value defines the number of iterations each ResponseMessage should contain
}
$(deriveJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> (toLower x):xs).(drop 12)} ''RequestArgs)
makeFields ''RequestArgs

-- | Format of requests to the Gremlin Server
data RequestMessage = RequestMessage {
   _requestMessageRequestId :: Text
   -- ^ A UUID representing the unique identification for the request.
   , _requestMessageOp :: Text
     -- ^ The name of the "operation" to execute based on the available OpProcessor configured in the Gremlin Server. To evaluate a script, use eval.
   , _requestMessageProcessor :: Text
     -- ^ The name of the OpProcessor to utilize. The default OpProcessor for evaluating scripts is unamed and therefore script evaluation purposes, this value can be an empty string.
   , _requestMessageArgs :: RequestArgs
     -- ^ Parameters to pass to Gremlin Server
}
$(deriveJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> (toLower x):xs).(drop 15)} ''RequestMessage)
makeFields ''RequestMessage

-- | The staus of Gremlin Server Response
data ResponseStatus = ResponseStatus {
    _responseStatusMessage :: Text
    -- ^ Human-readable String usually associated with errors.
    , _responseStatusCode :: Int
      -- ^ HTTP status code
    , _responseStatusAttributes :: Object
      -- ^ Protocol-level information
} deriving (Show)
makeFields ''ResponseStatus
$(deriveJSON defaultOptions {fieldLabelModifier = (\(x:xs) -> (toLower x):xs).(drop 15)} ''ResponseStatus)

-- | The Result of Gremlin Server response
data ResponseResult = ResponseResult {
    _responseResultData' :: Maybe [Value]
    -- ^ the actual data returned from the server (the type of data is determined by the operation requested)
    , _responseResultMeta :: Object
      -- ^ Map of meta-data related to the response.
} deriving (Show)
makeFields ''ResponseResult
$(deriveToJSON defaultOptions  {fieldLabelModifier = \f -> if f == "_responseResultData'" then "data" else (\(x:xs) -> (toLower x):xs) (drop 15 f)} ''ResponseResult)

instance FromJSON ResponseResult where
  parseJSON (Object o) = do
    mgvalue <- o .: "data"
    ResponseResult (_atValue <$> mgvalue) <$> (o .: "meta")
  parseJSON _ = mzero


-- | Response of Gremlin Server
data ResponseMessage = ResponseMessage {
    _responseMessageRequestId :: Text
    -- ^ The identifier of the RequestMessage that generated this ResponseMessage.
    , _responseMessageStatus :: ResponseStatus
      -- ^ status
    , _responseMessageResult :: ResponseResult
      -- ^ result
} deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = (\(x:xs) -> (toLower x):xs).(drop 16)} ''ResponseMessage)
makeFields ''ResponseMessage

-- | Connection handle
data Connection = Connection {
    _connectionSocket :: WS.Connection
    , _connectionChans :: S.TVar (M.Map Text (S.TChan (Either String ResponseMessage)))
}
makeFields ''Connection

