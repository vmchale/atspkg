ci:
    stack build
    cabal new-build
    hlint .
    weeder .

install:
    @cabal new-build
    @cp $(fd 'atspkg$' -IH dist-newstyle | tail -n1) ~/.local/bin
