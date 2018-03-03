#!/usr/bin/env bash

set -e
set pipefail

function getTarget {
    if [ "$(uname)" = "Darwin" ]
    then
        echo "atspkg-$(uname -m)-apple-darwin"
    else
        echo "atspkg-$(uname -m)-unknown-linux"
    fi
}

main() {

    latest="$(curl -s https://github.com/vmchale/atspkg/releases/latest/ | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    binname=$(getTarget)
    url="https://github.com/vmchale/atspkg/releases/download/$latest/$binname"
    mkdir -p "$HOME/.local/bin"
    local dest=$HOME/.local/bin/atspkg
    if which duma > /dev/null ; then
        duma "$url" -O "$dest"
    elif which wget > /dev/null ; then
        wget "$url" -O "$dest"
    else
        curl "$url" -o "$dest"
    fi
    chmod +x "$dest"

}

main
