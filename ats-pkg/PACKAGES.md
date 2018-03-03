# Making Your Own Packages

## Specifying NPM Packages

As many packages are already on
[NPM](https://www.npmjs.com/search?q=atscntrb-&page=1&ranking=optimal), the
`atspkg` Dhall
[prelude](https://github.com/vmchale/atspkg/blob/master/ats-pkg/dhall/atspkg-prelude.dhall)
provides a convenience function `makeNpmPkg`. You can use it like so:

```dhall
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall
in

let intinf =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-intinf", unpackDir = "atscntrb-hx-intinf" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-libgmp" ] }
in intinf
```

Note also the use of `mapPlainDeps` - this simply says that we should depend on
a package named `atscntrb-libgmp` but that we should not enforce any version
constraints.

## Specifying Other Packages

As `atspkg` simply needs a URL to make a package, you can create a packages
hosted on github. In this example, we tag the repo to
create tarballs for each version.

```
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall

in λ(x : List Integer) → 
  prelude.makePkgDescr
    { x = x
    , name = "either"
    , githubUsername = "vmchale"
    , description = "Generic sum types and utilities for working with them."
    }
```

## Making Package Sets

A package set is simply a list of packages in Dhall.
The default package set is
[`pkg-set.dhall`](https://github.com/vmchale/atspkg/blob/master/ats-pkg/pkgs/pkg-set.dhall),
however, the user may select a custom package set if desired, but editing their
`$HOME/.config/atspkg/config.dhall`.

```dhall
let version = "master"
in

let cfg =
  { defaultPkgs = "https://raw.githubusercontent.com/vmchale/atspkg/${version}/ats-pkg/pkgs/pkg-set.dhall"
  , path = ([] : Optional Text)
  , githubUsername = "vmchale"
  , filterErrors = False
  }
in cfg
```

The `deaultPkgs` field can take a filepath or a URL, so long as it is
a valid Dhall expression. As it is a Dhall expression, users can also
concatenate package sets from multiple sources if so desired.
