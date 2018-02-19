# hs2ats

This is a tool to convert Haskell types to ATS types.

Example use:

```
hs2ats --src DataTypes.hs --target generated_types.sats
```

Note that `hs2ats` does not preserve strictness semantics.
