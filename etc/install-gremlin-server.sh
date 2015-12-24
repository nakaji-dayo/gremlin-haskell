#!/bin/bash

# TODO: use docker

cd
if [ ! -e "apache-gremlin-server-3.1.0-incubating-bin" ]; then
    curl -L -O https://www.apache.org/dist/incubator/tinkerpop/3.1.0-incubating/apache-gremlin-console-3.1.0-incubating-bin.zip
    unzip apache-gremlin-server-3.1.0-incubating-bin.zip
fi
cd apache-gremlin-server-3.1.0-incubating-bin
bin/gremlin-server.sh conf/gremlin-server-modern.yaml &
