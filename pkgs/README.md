# Packages

This directory contains several package definitions I use to write ATS.

## Writing Your Own Packages

Here are the contents of `atscntrb-hx-libpcre.dhall`. As you can see, defining
a package is relatively simple: all it requires is a tarball and version
information.

```
let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-pkg.dhall

in dep //
  { libName = "atscntrb-hx-libpcre"
  , dir = ".atspkg/contrib/atscntrb-hx-libpcre"
  , url = "https://registry.npmjs.org/atscntrb-hx-libpcre/-/atscntrb-hx-libpcre-1.0.2.tgz"
  , libVersion = [0,1,2]
  }
```

`atspkg` defines `$PATSHOMELOCS` to be `.atspkg/contrib` and subdirectories, so
keep this in mind when choosing an unpack `dir`.

## Using Packages

As Dhall is distributed, you can simply point to the package configuration URL
to add a dependency. 

As a minimal example:

```
let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall

in pkg //
  { bin =
    [ dbin //
      { src = "src/project.dats"
      , target = "target/project"
      }
    ]
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall ]
  }
```

ATS Dependencies are for a whole project; you cannot yet set dependencies on
a per-binary basis.
