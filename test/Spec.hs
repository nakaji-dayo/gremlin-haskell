import Database.TinkerPop
import Data.Aeson.QQ

main :: IO ()
main = do
    run "localhost" 8182 $ \conn -> do
        res <- send conn sampleJSON
        putStrLn $ show res

sampleJSON = [aesonQQ|
{ "requestId":"1d6d02bd-8e56-421d-9438-3bd6d0079ff1",
  "op":"eval",
  "processor":"",
  "args":{"gremlin":"g.V().has('name','marko').out('created').in('created').values('name')",
  "bindings":{"x":1},
  "language":"gremlin-groovy",
  "batchSize": 2
}}
|]

