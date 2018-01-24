# ats-pkg

This is a build system for ATS written in Haskell and configured with Dhall. It
is currently a work-in-progress. You can see a working example
[here](http://github.com/vmchale/polyglot).

## Example

To build a binary package from source, run

```bash
 $ atspkg remote https://github.com/vmchale/polyglot/archive/0.3.28.tar.gz
```

## Installation

Currently, the best way to install is

```
cabal new-install ats-pkg --symlink-bindir ~/.local/bin
```

Make sure that `~/.local/bin` is on your `PATH`.
