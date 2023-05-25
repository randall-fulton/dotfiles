#!/bin/sh

if ! command -v npm >/dev/null; then
    echo "\033[31mnpm is not installed\033[0m"
fi

npm install -g @mermaid-js/mermaid-cli
