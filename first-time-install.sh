#!/bin/bash

set -xeuf -o pipefail

sudo apt-get update

sudo apt-get install curl libgrpc-dev sqlite3

if [[ -x "$(command -v stack)" ]]; then
    echo "Stack is already installed: Upgrading..."
    stack upgrade
else
    curl -sSL https://get.haskellstack.org/ | sh
fi
