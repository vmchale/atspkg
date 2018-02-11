let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-pkg.dhall
in
let concat = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/Text/concat

in λ(x : List Integer) → 
  dep x //
    { libName = "fast-arithmetic"
    , dir = ".atspkg/contrib/hspkg-fast-arthimetic"
    , url = "https://hackage.haskell.org/package/fast-arithmetic-", showVersion x, "/fast-arithmetic-", showVersion x, ".tar.gz"
    }
