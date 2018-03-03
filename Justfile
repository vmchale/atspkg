latex:
    cd ats-pkg/docs && pdflatex manual.tex && pdflatex manual.tex

darcs:
    darcs optimize clean
    darcs optimize pristine
    darcs optimize cache

fmt-install:
    @cabal new-build ats-format
    @cp ats-format/man/atsfmt.1 ~/.local/share/man/man1
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin

approve FILE:
    @atsfmt language-ats/test/data/{{ FILE }} -o > language-ats/test/data/$(echo {{ FILE }} | sed 's/\(dats\|hats\|sats\)/out/')
    sed -i '$d' language-ats/test/data/$(echo {{ FILE }} | sed 's/\(dats\|hats\|sats\)/out/')

clean:
    sn c .
    rm -rf tags ats-pkg/docs/manual.out

diff FILE:
    @diff <(atsfmt language-ats/test/data/{{ FILE }} -o) language-ats/test/data/$(echo {{ FILE }} | sed 's/\(dats\|hats\|sats\)/out/') | ac -s

manpages:
    pandoc ats-format/man/MANPAGE.md -s -t man -o ats-format/man/atsfmt.1
    pandoc ats-pkg/man/MANPAGE.md -s -t man -o ats-pkg/man/atspkg.1

debian:
    PATH=/usr/bin:$PATH cabal-debian --maintainer "Vanessa McHale <vamchale@gmail.com>"

poly:
    @poly -e data

ci: install
    @cabal new-test all
    yamllint .stylish-haskell.yaml
    yamllint .hlint.yaml
    yamllint .yamllint
    yamllint ats-pkg/stack.yaml
    yamllint ats-format/stack.yaml
    tomlcheck --file ats-format/.atsfmt.toml
    shellcheck ats-pkg/bash/install.sh
    hlint ats-pkg language-ats shake-ext ats-format
    stack build --test --no-run-tests --bench --no-run-benchmarks && weeder .
    atspkg nuke
    atspkg remote https://github.com/vmchale/polyglot/archive/master.zip

pkg-install:
    @cabal new-build all
    @cp -f $(fd 'atspkg$' -IH dist-newstyle | tail -n1) ~/.local/bin

install: fmt-install pkg-install

size:
    @sn d $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) $(fd 'atspkg$' -IH dist-newstyle | tail -n1)

test-format:
    @git clone https://github.com/vmchale/polyglot
    cd polyglot && atsfmt src/polyglot.dats -i
    cd polyglot && atsfmt src/cli.dats -i
    cd polyglot && atsfmt src/shared.dats -i
    cd polyglot && atsfmt src/filetype.sats -i
    cd polyglot && atspkg build
    @rm -rf polyglot
