latex:
    cd ats-pkg/docs && pdflatex manual.tex && pdflatex manual.tex

darcs:
    darcs optimize clean
    darcs optimize pristine
    darcs optimize cache

build:
    @cabal new-build all

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

dhall-check:
    atspkg check-set ats-pkg/pkgs/pkg-set.dhall
    cat ats-pkg/dhall/atslib.dhall | dhall
    cat ats-pkg/dhall/config.dhall | dhall
    cat ats-pkg/dhall/atspkg-prelude.dhall | dhall

ci: install
    @cabal new-test all
    shellcheck bash/install.sh
    yamllint .stylish-haskell.yaml
    yamllint .hlint.yaml
    yamllint .yamllint
    yamllint stack.yaml
    tomlcheck --file ats-format/.atsfmt.toml
    hlint ats-pkg language-ats shake-ext ats-format cdeps
    stack build --test --no-run-tests --bench --no-run-benchmarks && weeder .
    atspkg nuke
    atspkg remote https://github.com/vmchale/polyglot/archive/master.zip

# atspkg remote https://hackage.haskell.org/package/fast-arithmetic-0.3.3.5/fast-arithmetic-0.3.3.5.tar.gz
install: build
    @cp ats-format/man/atsfmt.1 ~/.local/share/man/man1
    @strip $(fd 'atsfmt$' -IH dist-newstyle | tail -n1)
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp -f $(fd 'atspkg$' -t x -IH dist-newstyle) ~/.local/bin

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
