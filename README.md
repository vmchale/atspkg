# ATSPackage

[![Build Status](https://travis-ci.org/vmchale/atspkg.svg?branch=master)](https://travis-ci.org/vmchale/atspkg)

This is a build system for ATS written in Haskell and configured with Dhall. It
is not fully working, but the configuration format is now stable.

## Features & Non-Features

Things that `atspkg` will do for you:

  * Dramatically simplify CI for ATS projects
  * Simplify distribution of your project
  * Enable Haskell builds that depend on ATS code
  * Enable ATS builds that depend on Haskell code
  * Ensure reproducible builds via pinned compiler versions
  * Track all file dependencies
  * Make contributing to your projects easier
  * Run builds in parallel (like `make`)
  * Handle flags and libraries for garbage collection when specified
  * Install `patscc` and other ATS tooling
  * Install manual pages and shell completions
  * Install ATS libraries
  * Cross-compile ATS

Things that `atspkg` will not do for you:

  * Dependency resolution (this is planned)
  * Give you the full flexibility of the C/ATS ecosystem
  * Integrate with other ecosystems
  * Provide a centralized package repository
  * Offer a common architecture for package builds
  * Cache builds locally (like `nix` or `cabal`)

### Example

As an example, the following two lines will install `polyglot`:

```bash
curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
atspkg remote https://github.com/vmchale/polyglot/archive/master.zip
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

If that doesn't work, you can download
[Cabal](https://www.haskell.org/cabal/download.html) and
[GHC](https://www.haskell.org/ghc/download.html) and install with

```bash
cabal update
cabal new-install ats-pkg --symlink-bindir ~/.local/bin --happy-options='-gcsa' --alex-options='-g'
```

Note that `$HOME/.local/bin` will need to be on your `PATH`.

## Quick Start

Install [pi](http://github.com/vmchale/project-init) with

```
curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git vmchale/project-init
```

Initialize an ATS project with

```
pi new ats project
```

Then build & run it:

```
cd project
atspkg run
```

## Examples

You can find several examples with explanation
[here](https://github.com/vmchale/atspkg/blob/master/EXAMPLES.md)

## Global Configuration

`atspkg` is configured via a file in `~/.config/atspkg/config.dhall`. You can
set a custom package set as follows:

```
let cfg = 
  { defaultPkgs = "https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/pkg-set.dhall"
  , path = ([] : Optional Text)
  , githubUsername = "YOUR_USERNAME"
  }

in cfg
```
