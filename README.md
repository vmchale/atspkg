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

### Building a Binary Package

To build a binary package from a source tarball:

```bash
 $ atspkg remote https://github.com/vmchale/polyglot/archive/0.3.34.tar.gz
```
