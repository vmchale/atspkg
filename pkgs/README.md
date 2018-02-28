# Packages

This directory contains several package definitions I use to write ATS.

## Writing Your Own Packages

Here are the contents of `atscntrb-hx-libpcre.dhall`. As you can see, defining
a package is relatively simple: all it requires is a tarball and version
information.

```
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in prelude.dep //
  { libName = "atscntrb-hx-libpcre"
  , dir = "${prelude.patsHome}/atscntrb-hx-libpcre"
  , url = "https://registry.npmjs.org/atscntrb-hx-libpcre/-/atscntrb-hx-libpcre-1.0.2.tgz"
  , libVersion = [0,1,2]
  }
```

## Using Packages

To use a package, simply call it by its name and ensure the package is listed in
the package set you are using.

As a minimal example:

```
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/prelude.dhall

in prelude.default //
  { bin =
    [ prelude.bin //
      { src = "src/project.dats"
      , target = "target/project"
      }
    ]
  , dependencies = prelude.mapPlainDeps [ "ats-concurrency" ]
  }
```

ATS Dependencies are for a whole project; you cannot yet set dependencies on
a per-binary basis.
