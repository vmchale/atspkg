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

### Project Templates

You can use [pi](https://github.com/vmchale/project-init) with the builtin `ats`
template as follows:

```
pi new ats cool-project
```

You can then build with `atspkg build` or install with `atspkg install`.

Alternately, you can start with a templated Haskell library calling ATS code:

```
pi git vmchale/haskell-ats ambitious-project
```

which can be built with `atspkg build` followed by `cabal new-build`.

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
ATS source files and track them (it will not track included C however).

### Depending on a Library

Library specifications are also written in Dhall. Let's look at an example:

```dhall
let dep =
  { libName = "atscntrb-concurrency-0.1.0"
  , dir = ".atspkg/contrib"
  , url = "https://github.com/vmchale/ats-concurrency/archive/0.1.0.tar.gz"
  , libVersion = [0,1,0]
  }

in dep
```

This defines a dependency by pointing to its tarball. Unlike
[cabal](https://www.haskell.org/cabal/) or other sophisticated package managers,
`atspkg` does not allow transitive dependencies and it does not do any
constraint solving. Let's look at a simple example:

```
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { bin =
    [
      { src = "src/compat.dats"
      , target = "target/poly"
      , libs = ([] : List Text)
      , gc = True
      }
    ]
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall ]
  }
```

As Dhall is distributed, you can simply point to the package configuration URL
to add a dependency. You can find several preconfigured packages
[here](https://github.com/vmchale/atspkg/tree/master/pkgs), or you can write
your own configurations.

### Building a Haskell Library

You can see an example [here](https://github.com/vmchale/fast-arithmetic). You
can

```
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { atsSource = [ "ats-src/{{ project }}.dats" ] }
```
