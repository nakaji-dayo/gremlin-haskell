import Database.TinkerPop
import Database.TinkerPop.Types
import Test.Hspec
import Data.Aeson.QQ
import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Control.Concurrent.MVar

main :: IO ()
main = hspec $ do
  describe "submit" $ do
      it "return result" $ do
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
      it "return error" $ do
          run "localhost" 8182 $ \conn -> do
              Left msg <- submit conn "throw new RuntimeException('MyError')"
              msg `shouldBe` "MyError"
