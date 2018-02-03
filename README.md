# ats-pkg

[![Build Status](https://travis-ci.org/vmchale/atspkg.svg?branch=master)](https://travis-ci.org/vmchale/atspkg)

This is a build system for ATS written in Haskell and configured with Dhall. It
is not yet stable.

## Installation

The easiest way to install is via a script, viz.

```bash
curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
```

Alternately, you can download
[Cabal](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html) and install with

```bash
cabal new-install ats-pkg ~/.local/bin --happy-options='-gcsa' --alex-options='-g'
```

Note that `$HOME/.local/bin` will need to be on your `PATH`.

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
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall

in pkg //
  { bin =
    [ dbin //
      { src = "src/program.dats"
      , target = "target/program"
      }
    ]
  }
```

You need only specify the source file and the target; `atspkg` will parse your
ATS source files and track them (it will not track included C).

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

This defines a dependency by pointing to its tarball. Let's look at a simple
example:

```
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall

in pkg //
  { bin =
    [ dbin //
      { src = "src/compat.dats"
      , target = "target/poly"
      }
    ]
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall ]
  }
```

As Dhall is distributed, you can simply point to the package configuration URL
to add a dependency. You can find several preconfigured packages
[here](https://github.com/vmchale/atspkg/tree/master/pkgs), or you can write
your own configurations. You can even make package lists that are later filtered
if you so choose.

### Building a Haskell Library

You can see an example [here](https://github.com/vmchale/fast-arithmetic). You
can configure the ATS side of things as follows:

```
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { atsSource = [ "ats-src/ambitious-project.dats" ] }
```

This just tells `atspkg` to look for a source file called
`ats-src/ambitious-project.dats`, which will be compiled to
`ambitious-project.c` in the default directory (i.e. `cbits`). You can then
call the generated code just as you would call C.

You may want to consider
[ats-setup](http://hackage.haskell.org/package/ats-setup) as well if you are
packaging the Haskell for distribution.

### Calling Haskell from ATS

You can see a demo [here](https://github.com/vmchale/fast-arithmetic).
Currently, there is not generic `Storable` instance that works with ATS, so the
process is a bit more involved than is ideal. `atspkg` has abilities similar to
[hs2ats](http://hackage.haskell.org/package/hs2ats), which means that you can
usually generate ATS types based on the Haskell types.

The following is a minimal example of a configuration file:

```dhall
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall

in pkg //
  { bin =
    [
      dbin //
      { src = "src/project.dats"
      , target = "target/project"
      , hsDeps = [ { cabalFile = "hs/foreign.cabal", objectFile = "hs/Foreign.o" } ]
      , hs2ats = [ { hs = "hs/Foreign.hs", ats = ".atspkg/hs2ats/gen.sats" } ]
      }
    ]
    , ccompiler = "ghc-8.2.2"
    , cflags = ["-package-db", "hs/dist-newstyle/packagedb/ghc-8.2.2/", "-optc-O2", "-optc-flto", "-optc-mtune=native", "hs/Foreign"]
  }
```
