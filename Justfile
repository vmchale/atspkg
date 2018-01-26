install:
    cd ../ && just install

test: install
    atspkg remote https://github.com/vmchale/polyglot/archive/0.3.34.tar.gz

ci: test
    stack build
    hlint .
    weeder .
