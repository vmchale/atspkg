latex:
    cd ats-pkg/docs && pdflatex manual.tex && pdflatex manual.tex

darcs:
    darcs optimize clean
    darcs optimize pristine
    darcs optimize cache


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

chall-check:
    atspkg check-set ats-pkg/pkgs/pkg-set.dhall
    cat ats-pkg/dhall/atslib.dhall | dhall
    cat ats-pkg/dhall/config.dhall | dhall
    cat ats-pkg/dhall/atspkg-prelude.dhall | dhall

ci: install
    @cabal new-test all
    shellcheck -e SC2016 bash/install.sh
    shellcheck bash/upload
    yamllint .stylish-haskell.yaml
    yamllint .hlint.yaml
    yamllint .yamllint
    yamllint .travis.yml
    yamllint appveyor.yml
    tomlcheck --file ats-format/.atsfmt.toml
    hlint ats-pkg language-ats ats-format shake-cabal shake-c

profile:
    @cabal new-build all -p --enable-profiling
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp -f $(fd 'atspkg$' -t x -IH dist-newstyle | tail -n1) ~/.local/bin

install:
    @cabal new-build all
    @strip $(fd 'atsfmt$' -IH dist-newstyle | tail -n1)
    @cp ats-format/man/atsfmt.1 ~/.local/share/man/man1
    @cp $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) ~/.local/bin
    @cp -f $(fd 'atspkg$' -t x -IH dist-newstyle | tail -n1) ~/.local/bin

size:
    @sn d $(fd 'atsfmt$' -IH dist-newstyle | tail -n1) $(fd 'atspkg$' -IH dist-newstyle | tail -n1)
