#!/bin/bash

export ROOT="$( readlink -f "$( dirname "${BASH_SOURCE[0]}" )" )"

rm -rf ${ROOT}/dist

cd ${ROOT}/hsMonitor
stack install --pedantic --local-bin-path=${ROOT}/dist

cd ${ROOT}/hsAutoscaling
stack install --pedantic --local-bin-path=${ROOT}/dist
