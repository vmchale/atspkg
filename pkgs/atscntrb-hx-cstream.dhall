let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let concat = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/Text/concat
in
let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/dhall-version.dhall

in λ(x : List Integer) → 
  dep //
    { libName = "atscntrb-hx-cstream"
    , dir = ".atspkg/contrib/atscntrb-hx-cstream"
    , url = concat ["https://registry.npmjs.org/atscntrb-hx-cstream/-/atscntrb-hx-cstream-", showVersion x, ".tgz"]
    , libVersion = x
    }
