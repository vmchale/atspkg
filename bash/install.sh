#!/usr/bin/env sh

set -e
set pipefail

getTarget() {
    if [ "$(uname)" = "Darwin" ]
    then
        echo "atspkg-$(uname -m)-apple-darwin"
    else
        echo "atspkg-$(uname -m)-unknown-linux"
    fi
}

addBin() {

    printf 'export PATH=$HOME/.local/bin:$PATH' >> "$HOME"/.bashrc
    export PATH=$HOME/.local/bin:$PATH

}

main() {


    mkdir -p "$HOME/.local/bin"
    mkdir -p "$HOME/.local/share/man/man1/"

    latest="$(curl -Ls -o /dev/null -w %\{url_effective\} https://github.com/vmchale/atspkg/releases/latest | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    binname=$(getTarget)

    url="https://github.com/vmchale/atspkg/releases/download/$latest/$binname"
    man_url="https://github.com/vmchale/atspkg/releases/download/$latest/atspkg.1"

    man_dest=$HOME/.local/share/man/man1/atspkg.1
    dest=$HOME/.local/bin/atspkg

    if command -v wget > /dev/null ; then
        wget "$url" -O "$dest"
        wget "$man_url" -O "$man_dest"
    else
        curl -L "$url" -o "$dest"
        curl -L "$man_url" -o "$man_dest"
    fi

    chmod +x "$dest"

    case :$PATH: in 
        *:$HOME/.local/bin:*) ;;
        *) addBin ;;
    esac

}

main
