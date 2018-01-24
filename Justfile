download:
    duma https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-0.3.8/ATS2-Postiats-0.3.8.tgz

ci:
    stack build
    cabal new-build
    hlint .
    weeder .

install:
    @cabal new-build
    @cp $(fd 'atspkg$' -IH dist-newstyle | tail -n1) ~/.local/bin
