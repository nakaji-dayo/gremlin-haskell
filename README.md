# gremlin-haskell
Haskell graph database client for TinkerPop3 Gremlin Server

## test
### to run the test with gremlin-server

```
# download https://www.apache.org/dist/incubator/tinkerpop/3.1.0-incubating/apache-gremlin-server-3.1.0-incubating-bin.zip
bin/gremlin-server.sh conf/gremlin-server-modern.yaml
```

### run test
```
stack test
```

## TODO:
- Handle errors
- Use as streaming data source
- fix ```gremlin-haskell-test: threadWait: invalid argument (Bad file descriptor)```
