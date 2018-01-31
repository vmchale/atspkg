install:
    cd ../ && just install

test: install
    atspkg nuke
    atspkg remote https://github.com/vmchale/polyglot/archive/0.3.36.tar.gz

ci:
    stack build
    hlint .
    weeder .
