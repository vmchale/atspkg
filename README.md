# ats-pkg

This is a build system for ATS written in Haskell and configured with Dhall. It
is currently a work-in-progress. You can see a working example
[here](http://github.com/vmchale/polyglot).

## Installation

Currently, the best way to install is

```
cabal new-install atspkg --symlink-bindor ~/.local/bin
```

Make sure that `~/.local/bin` is on your `PATH`.
