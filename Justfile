install:
    @cabal new-build
    @cp $(fd 'atspkg$' -IH dist-newstyle | tail -n1) ~/.local/bin
