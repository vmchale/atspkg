let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-pkg.dhall

in dep //
  { libName = "hs-dhall"
  , dir = "hs2ats-deps/dhall-1.9.1"
  , url = "https://hackage.haskell.org/package/dhall-1.9.1/dhall-1.9.1.tar.gz"
  , libVersion = [1,9,1]
  }
