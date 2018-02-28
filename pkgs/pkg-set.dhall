{- Import the atspkg prelude -}
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall
in

{- Packages -}
let fastArithmetic =
  λ(x : List Integer) → 
    prelude.makeHsPkg { x = x, name = "fast-arithmetic" } 
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-intinf" ], description = [ "Number theory & combinatorics library written in ATS" ] : Optional Text }
in

{-
let divideConquer =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquer" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ] }
in
-}

let pkgset =
  [ https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquer.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquerpar.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-fworkshop.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-threadkit.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-intinf.dhall [1,0,8]
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-libgmp.dhall [1,0,4]
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-cstream.dhall [1,0,4]
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/libc-atomic-ops.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/libc-gc.dhall
  , fastArithmetic [0,3,3,0]
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/pkg.dhall [0,4,3]
  , https://raw.githubusercontent.com/vmchale/hs-bind/master/pkg.dhall
  , https://raw.githubusercontent.com/vmchale/nproc-ats/master/pkg.dhall [0,1,5]
  , https://raw.githubusercontent.com/vmchale/either/master/pkg.dhall [0,2,1]
  , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall [0,2,2]
  ]

in pkgset
