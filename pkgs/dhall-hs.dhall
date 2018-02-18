let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall

in dep //
  { libName = "hs-dhall"
  , dir = ".atspkg/hs2ats-deps"
  , url = "https://hackage.haskell.org/package/dhall-1.9.1/dhall-1.9.1.tar.gz"
  , libVersion = [1,9,1]
  }
