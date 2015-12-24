module Main where

import Database.TinkerPop
import Database.TinkerPop.Types
import Control.Concurrent
import Control.Monad.Trans (liftIO)


main :: IO ()
main = do
    run "localhost" 8182 $ \conn -> do
        a <- submit conn "g.V().drop()"
        print a
        res <- submit conn "graph.addVertex(label, 'language', 'name', 'haskell')"
        print res
