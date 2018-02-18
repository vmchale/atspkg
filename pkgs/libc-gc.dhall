let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in dep //
  { libName = "gc"
  , dir = "gc-7.6.4"
  , url = "https://github.com/ivmai/bdwgc/releases/download/v7.6.4/gc-7.6.4.tar.gz"
  , libVersion = [7,6,4]
  , libDeps = prelude.mapPlainDeps [ "atomic-ops" ]
  }
