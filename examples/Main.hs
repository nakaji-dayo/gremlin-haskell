module Main where

import Database.TinkerPop
import Database.TinkerPop.Types

import Data.Text
import Data.Aeson
import Data.Aeson.Types

import qualified Data.HashMap.Strict as H
import Control.Concurrent
import Control.Monad

import Control.Monad.Trans (liftIO)

import Control.Lens hiding ((.=), (.:))
import Data.Aeson.Lens


main :: IO ()
main = do
    run "localhost" 8182 $ \conn -> do
        -- DROP Database
        submit conn "g.V().drop()" Nothing >>= print
        let addV = "graph.addVertex(label, 'language', 'name', n)"
        -- add 'haskell' vertex
        haskell <- submit conn addV (Just $ H.fromList ["l" .= ("language" :: Text), "n" .= ("haskell" :: Text)])
        print haskell
        let idHaskell = getId haskell 
        -- add (library) vertexes
        yesod <- submit conn addV (Just $ H.fromList ["l" .= ("library" :: Text), "n" .= ("yesod" :: Text)])
        print yesod
        let idYesod = getId yesod 
        idAeson <- getId <$> submit conn addV (Just $ H.fromList ["l" .= ("library" :: Text), "n" .= ("aeson" :: Text)]) 
        idLens <- getId <$> submit conn addV (Just $ H.fromList ["l" .= ("library" :: Text), "n" .= ("lens" :: Text)])
        -- add (library -written-> language) edge
        mapM (\lib -> submit conn "g.V(from).next().addEdge('written', g.V(to).next())" (Just $ H.fromList ["from" .= lib, "to" .= idHaskell])) [idYesod, idHaskell, idLens] >>= print
        -- query
        submit conn "g.V().has('name', 'haskell').in('written').values()" Nothing >>= print
    where getId = (^? _Right . element 0 . key "id" . _Integer)
