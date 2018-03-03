# ats-formatter

[![Build Status](https://travis-ci.org/vmchale/ats-format.svg?branch=master)](https://travis-ci.org/vmchale/ats-format)

<img alt="Screenshot of sample results" src=https://github.com/vmchale/ats-format/raw/master/atsfmt.png>
<img alt="Screenshot of sample results" src=https://github.com/vmchale/ats-format/raw/master/atsfmt2.png>

This is a code formatter for [ATS](http://www.ats-lang.org/). It is
a work-in-progress.

If you find something that's not listed in `TODO.md` feel free to open
an issue. Code samples that were formatted into something ugly are also welcome.

## Configuration

`atsfmt` is configured with the `.atsfmt.toml` file. You can generate a default
configuration with

```bash
 $ atsfmt --default-config
```

### Vim

You can use [this](https://github.com/vmchale/ats-vim) plugin to enable
automatic formatting on write.

## Installation

### Binary Releases

The [releases](https://github.com/vmchale/ats-format/releases) page has binary
releases for common platforms.

### Compilation from Source

To install, first install [GHC](https://www.haskell.org/ghc/download.html), then
[cabal](https://www.haskell.org/cabal/download.html). Then

```bash
 $ cabal update
 $ cabal new-install ats-format --happy-options='-gcsa' --alex-options='-g' --symlink-bindir ~/.local/bin -O2
```

## License

All code except `test/data/left-pad.dats` is licensed under the BSD3 license.
