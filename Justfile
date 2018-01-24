test: install
    rm -rf polyglot
    git clone git\@github.com:vmchale/polyglot.git
    cd polyglot && atspkg test

download:
    duma https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-0.3.9/ATS2-Postiats-0.3.9.tgz

ci: test
    stack build
    hlint .
    weeder .

install:
    @sn c .
    @cabal new-build
    @cp -f $(fd 'atspkg$' -IH dist-newstyle | tail -n1) ~/.local/bin
