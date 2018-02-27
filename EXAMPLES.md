# Examples

`atspkg` is configured with
[Dhall](https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html). You
may wish to read the Dhall tutorial first, but you do not need to fully
understand everything to get started.

## Project Templates

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

### Other Templates

  * ATS project calling Haskell:
    [ats-haskell](https://github.com/vmchale/ats-haskell)

## Samples

  * Executable project: [polyglot](https://github.com/vmchale/polyglot)
  * Library and test suite: [either](http://github.com/vmchale/either)
  * ATS calling Haskell: [dhall-ats](https://github.com/vmchale/dhall-ats)

## Building a Binary Package

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

## Building a Static Library

Building a static library is quite similar:

```dhall
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in prelude.default //
  { libraries =
    [
      prelude.lib //
      { name = "concurrency"
      , src = [ "mylibies_link.hats" ]
      , includes = [ "mylibies_link.hats", ".atspkg/contrib/channel_link.hats" ]
      , links = [ { _1 = "channel.sats", _2 = ".atspkg/contrib/channel_link.hats" } ]
      , libTarget = ".atspkg/lib/libconcurrency.a"
      , libs = [ "pthread" ]
      , static = True
      }
    ]
  , dependencies = prelude.mapPlainDeps [ "nproc-ats" ]
  }
```

Note the ability of `atspkg` to generate `.hats` files from `.sats` files -
you can use a static file to declare the types in your library, and then
compile to a static library separately. This is enormously beneficial for large
builds, where we can take advantage of multicore processors to dependencies in
parallel rather than splattering `#include` directives all over our projects.

You can see the full example [here](https://github.com/vmchale/ats-concurrency).

## Building a Haskell Library

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

## Calling Haskell from ATS

You can see a demo [here](https://hub.darcs.net/vmchale/ats-storable/browse/dhall-ats).
Currently, there is not generic `Storable` instance that works with ATS, so the
process is a bit more involved than is ideal. `atspkg` has abilities similar to
[hs2ats](http://hackage.haskell.org/package/hs2ats), which means that you can
usually generate ATS types based on the Haskell types.

The following is a minimal example of a configuration file:

```dhall
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default.dhall
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-bin.dhall

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

## Writing Your Own Packages

Have a look [here](https://github.com/vmchale/either) for
an example package using some of the Dhall helper functions provided to manage
versioning.

The sample configuration file is:

```dhall
let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/prelude.dhall

in λ(x : List Integer) → 
  prelude.makePkg { x = x, name = "either", githubUsername = "vmchale" }
    // { description = [ "Generic sum types and utilities for working with them." ] : Optional Text }
```
