# hs2ats

This is a tool to convert Haskell types to ATS types.

Example use:

```
hs2ats --src DataTypes.hs --target generated_types.sats
```

Note that `hs2ats` does not preserve strictness semantics.

## Installation

Install [cabal](https://www.haskell.org/cabal/download.html). Then:

```
cabal new-install hs2ats --symlink-bindir ~/.cabal/bin
```

or

```
cabal new-install hs2ats --alex-options='-g' --happy-options='-gcsa' --symlink-bindir ~/.cabal/bin
```
