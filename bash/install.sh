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


    mkdir -p "$HOME/.local/bin"
    mkdir -p "$HOME/.local/share/man/man1/"

    latest="$(curl -s https://github.com/vmchale/atspkg/releases/latest/ | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    binname=$(getTarget)

    url="https://github.com/vmchale/atspkg/releases/download/$latest/$binname"
    man_url="https://github.com/vmchale/atspkg/releases/download/$latest/atspkg.1"

    man_dest=$HOME/.local/share/man/man1/atspkg.1
    dest=$HOME/.local/bin/atspkg

    if which duma > /dev/null ; then
        duma "$url" -O "$dest"
        duma "$man_url" -O "$man_dest"
    elif which wget > /dev/null ; then
        wget "$url" -O "$dest"
        wget "$man_url" -O "$man_dest"
    else
        curl "$url" -o "$dest"
        curl "$man_url" -o "$man_dest"
    fi

    chmod +x "$dest"

}

main
