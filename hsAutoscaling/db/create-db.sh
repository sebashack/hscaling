#!/bin/bash

export ROOT="$( readlink -f "$( dirname "${BASH_SOURCE[0]}" )" )"

if [[ $* == "" ]]; then
    >&2 echo "No db filepath provided"
    exit 1
fi

DBFILE=$1

sqlite3 -init ${ROOT}/schema.sqlite ${DBFILE} .quit
