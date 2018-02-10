let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-pkg.dhall

in dep //
  { libName = "gmp"
  , dir = "gmp-6.1.2"
  , url = "https://gmplib.org/download/gmp/gmp-6.1.2.tar.xz"
  , libVersion = [6,1,2]
  }
