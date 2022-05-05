#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

runHpcThreshold() {
    if hash hpc-threshold 2>/dev/null; then
        tee >(hpc-threshold "$@")
    else
        if [ -x "$SCRIPTPATH/../bin/hpc-threshold" ]; then
            tee >($SCRIPTPATH/../bin/hpc-threshold "$@")
        else
            2>&2
        fi
    fi
}

runTests() {
    testTarget=$1

    cd $SCRIPTPATH/$testTarget
    stack --silent test --coverage neodiff:test:$testTarget && stack hpc report neodiff:test:$testTarget 2>&1 | runHpcThreshold
}
