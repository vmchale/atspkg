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
    mkdir -p "$HOME/.local/bin"
    local dest=$HOME/.local/bin/atspkg
    curl -Lo "$dest" https://github.com/vmchale/atspkg/releases/download/"$latest"/"$binname"
    chmod +x "$dest"

}

main
