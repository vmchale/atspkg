# hs2ats

This is a tool to convert Haskell types to ATS types. So far it works quite
well, but documentation is sparse.

Example use:

```
hs2ats --src DataTypes.hs --target generated_types.sats
```

Note also that `hs2ats` does not preserve strictness semantics.
