#!/bin/bash

# TODO: use docker

cd ~
if [ ! -e "gremlin-server" ]; then
    curl -L -O https://www.apache.org/dist/incubator/tinkerpop/3.1.0-incubating/apache-gremlin-server-3.1.0-incubating-bin.zip
    unzip apache-gremlin-server-3.1.0-incubating-bin.zip
    mv apache-gremlin-server-3.1.0-incubating gremlin-server
fi
cd gremlin-server
nohup ./bin/gremlin-server.sh ./conf/gremlin-server-modern.yaml &
sleep 3
