# ats-pkg

[![Build Status](https://travis-ci.org/vmchale/atspkg.svg?branch=master)](https://travis-ci.org/vmchale/atspkg)

This is a build system for ATS written in Haskell and configured with Dhall.

## Installation

The easiest way to install is via a script, viz.

```bash
 $ curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
```

Alternately, you can download
[Cabal](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html) and install with

```bash
 $ cabal new-install ats-pkg --symlink-bindir ~/.local/bin
```

## Examples

`atspkg` is configured with
[Dhall](https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html). You
may wish to read the Dhall tutorial first, but you do not need to fully
understand everything to get started.

### Building a Binary Package

The minimal configuration for a package with a binary target is as follows:

```dhall
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { bin =
    [
      { src = "src/program.dats"
      , target = "target/program"
      , gc = True
      }
    ]
  }
```

You need only specify the source file and the target; `atspkg` will parse your
ATS source files and track them.

### Depending on a Library

Library specifications are also written in Dhall. Let's look at an example:

```dhall
let dep =
  { libName = "atscntrb-concurrency-0.1.0"
  , dir = ".atspkg/contrib"
  , url = "https://github.com/vmchale/ats-concurrency/archive/0.1.0.tar.gz"
  }

in dep
```

### Building a Haskell Library

You can see an example [here](https://github.com/vmchale/fast-arithmetic). You
can

``
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { atsSource = [ "ats-src/{{ project }}.dats" ] }
```
