#!/bin/sh

if ! command -v go >/dev/null; then
    echo "\033[31mgo is not installed\033[0m"
fi

# analysis tool for project dependencies
go install github.com/Helcaraxan/gomod@v0.7.1

# diagramming alternative to graphviz
# releases: https://github.com/terrastruct/d2/releases
go install oss.terrastruct.com/d2@v0.4.2
