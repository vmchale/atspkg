# ATSPackage

[![Build Status](https://travis-ci.org/vmchale/atspkg.svg?branch=master)](https://travis-ci.org/vmchale/atspkg)

This is a build system for ATS written in Haskell and configured with Dhall. It
is not yet stable.

## Features & Non-Features

Things that `atspkg` will do for you:

  * Dramatically simplify CI for ATS projects
  * Simplify distribution of your project
  * Enable Haskell builds that depend on ATS code
  * Enable ATS builds that depend on Haskell code
  * Ensure reproducible builds by allowing pinning a compiler version
  * Track all file dependencies
  * Make contributing to your projects easier
  * Run builds in parallel (like `make`)
  * Handle flags and libraries for garbage collection when specified
  * Install `patscc` and other ATS tooling

Things that `atspkg` will not do for you:

  * Dependency resolution (this is planned)
  * Give you the full flexibility of the C/ATS ecosystem
  * Integrate with other ecosystems
  * Provide a centralized package repository
  * Library builds (this is planned)
  * Offer a principled foundation for package builds
  * Cache builds locally (like `nix` or `cabal`)

### Example

As an example, the following two lines will install `getkb`:

```bash
curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
atspkg remote https://github.com/vmchale/getkb/archive/master.zip
```

As you can see, this greatly simplifies distribution and testing of programs
written in ATS.

## Installation

### Script

The easiest way to install is via a script, viz.

```bash
curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
```

Thereafter, you can run

```bash
atspkg upgrade
```

to upgrade to the latest release.

### Source

Alternately, you can download
[Cabal](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html) and install with

```bash
cabal new-install ats-pkg --symlink-bindir ~/.local/bin --happy-options='-gcsa' --alex-options='-g'
```

Note that `$HOME/.local/bin` will need to be on your `PATH`.

## Examples

You can find several examples with explanation
[here](https://github.com/vmchale/atspkg/blob/master/EXAMPLES.md)
