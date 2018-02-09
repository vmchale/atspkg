let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-pkg.dhall

in dep //
  { libName = "fast-arithmetic"
  , dir = ".atspkg/contrib/hspkg-fast-arthimetic"
  , url = "https://hackage.haskell.org/package/fast-arithmetic-0.3.2.4/fast-arithmetic-0.3.2.4.tar.gz"
  , libVersion = [0,3,2,4]
  }
