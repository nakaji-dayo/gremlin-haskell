# gremlin-haskell
Haskell graph database client for TinkerPop3 Gremlin Server

https://hackage.haskell.org/package/gremlin-haskell

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
- Use as streaming data source
- Resolve thread problem ```gremlin-haskell-test: threadWait: invalid argument (Bad file descriptor)```
- Rewrite test
- create Gnerating Gremlin script DSL(?)
