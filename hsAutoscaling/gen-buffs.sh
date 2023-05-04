#!/bin/bash

export ROOT="$( readlink -f "$( dirname "${BASH_SOURCE[0]}" )" )"

stack exec -- compile-proto-file --includeDir ${ROOT}/../protos --proto grpc/protobuf/monitor.proto --out ${ROOT}/src
