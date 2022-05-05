#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

installTool() {
    nameOfTool=$1

    if hash $nameOfTool 2>/dev/null; then
        echo -e "\t* $nameOfTool [INSTALLED]"
        return
    fi
    if [ -x "$SCRIPTPATH/bin/$nameOfTool" ]; then
        echo -e "  * $nameOfTool [INSTALLED]"
        return
    fi

    echo -e "\t* Installing $nameOfTool"
    if [ ! -d "$SCRIPTPATH/bin" ]; then
        mkdir "$SCRIPTPATH/bin"
    fi
    stack install $nameOfTool --local-bin-path "$SCRIPTPATH/bin/"
}

echo "Setting up project"
echo "> Installing tools"
installTool hpc-threshold
