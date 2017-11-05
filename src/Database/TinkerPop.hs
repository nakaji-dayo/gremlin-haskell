-- |
-- Module: Database.TinkerPop
-- Copyright: (c) 2015 The gremlin-haskell Authors
-- License     : BSD3
-- Maintainer  : nakaji.dayo@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
module Database.TinkerPop where

import Database.TinkerPop.Types
import Database.TinkerPop.Internal

import Prelude
import qualified Network.WebSockets as WS
import Control.Exception

import qualified Data.Map.Strict as M
import Data.Text (unpack)
import Data.Aeson (encode, Value)
import qualified Control.Monad.STM as S
import qualified Control.Concurrent.STM.TChan as S
import qualified Control.Concurrent.STM.TVar as S
import qualified Control.Concurrent.MVar as MV
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Control.Lens

-- | Connect to Gremlin Server
run :: String -> Int -> (Connection -> IO ()) -> IO ()
run host port app = do
    handle (wsExceptionHandler "main thread") $ WS.runClient host port "/gremlin" $ \ws -> do
        done <- MV.newEmptyMVar
        cs <- S.newTVarIO M.empty
        let conn = Connection ws cs
        _ <- handler conn done
        app conn
        close conn
        MV.takeMVar done

-- | Send script toGremlin Server and get the result by List
--
-- Individual responses are combined in internal.
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

-- | Build request data
buildRequest :: Gremlin -> Maybe Binding -> IO RequestMessage
buildRequest body binding = do
    uuid <- U.toText <$> U.nextRandom
    return $ RequestMessage uuid "eval" "" $
        RequestArgs body binding "gremlin-groovy" Nothing
