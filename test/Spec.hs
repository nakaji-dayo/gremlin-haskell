import Database.TinkerPop
import Test.Hspec
import Data.Aeson.QQ
import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar

main :: IO ()
main = hspec $ do
  describe "submit" $ do
      it "sync return result" $ do
          run "localhost" 8182 $ \conn -> do
              Right res <- submit conn "g.V().values('name')"
              putStrLn $ show res
              length res `shouldBe` 6
      it "with multi thread" $ do
          run "localhost" 8182 $ \conn -> do
              var <- newEmptyMVar
              flip forkFinally (putMVar var) $ do
                  res <- submit conn "g.V().values('age')"
                  liftIO $ putStrLn $ show res
                  return res 
              Right res <- submit conn "g.V().has('name','marko').out('created').in('created').values('name')"
              putStrLn $ show res
              length res `shouldBe` 3              
              Right (Right threadRes) <- takeMVar var
              length threadRes `shouldBe` 4

-- main = do
--     run "localhost" 8182 $ \conn -> do
--         var <- newEmptyMVar
--         flip forkFinally (putMVar var) $ do
--             Right res <- submit conn "g.V().values('age')"
--             liftIO $ putStrLn $ show res
--             return res 
--         Right res <- submit conn "g.V().has('name','marko').out('created').in('created').values('name')"
--         putStrLn $ show res
--         threadRes <- takeMVar var
--         putStrLn $ show threadRes
