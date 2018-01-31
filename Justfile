install:
    cd ../ && just install

test: install
    atspkg nuke
    atspkg remote https://github.com/vmchale/polyglot/archive/0.3.35.tar.gz

ci: test
    stack build
    hlint .
    weeder .
