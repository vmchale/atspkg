let pkgset =
  [ https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquer.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquerpar.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-fworkshop.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-threadkit.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-cstream.dhall [1,0,4]
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/libc-atomic-ops.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/libc-gc.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/dhall-hs.dhall
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/pkg.dhall [0,3,4]
  , https://raw.githubusercontent.com/vmchale/hs-bind/master/pkg.dhall
  , https://raw.githubusercontent.com/vmchale/nproc-ats/master/pkg.dhall [0,1,0]
  , https://raw.githubusercontent.com/vmchale/either/master/pkg.dhall [0,2,0]
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-fast-arithmetic.dhall [0,3,2,5]
  , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall [0,1,2]
  ]

in pkgset
