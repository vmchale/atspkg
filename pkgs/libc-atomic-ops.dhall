let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall

in dep //
  { libName = "atomic-ops"
  , dir = "atomic-ops-7.6.2"
  , url = "https://github.com/ivmai/libatomic_ops/releases/download/v7.6.2/libatomic_ops-7.6.2.tar.gz"
  , libVersion = [7,6,2]
  }
