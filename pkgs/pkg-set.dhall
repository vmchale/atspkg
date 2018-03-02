{- Import the atspkg prelude -}
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall
in

{- Packages -}
let fastArithmetic =
  λ(x : List Integer) →
    prelude.makeHsPkg { x = x, name = "fast-arithmetic" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-intinf" ], description = [ "Number theory & combinatorics library written in ATS" ] : Optional Text }
in

let gmp =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscnrb-libgmp", unpackDir = "atscntrb-libgmp" }
in

let atomicOps =
  prelude.dep //
    { libName = "atomic-ops"
    , dir = "atomic-ops-7.6.2"
    , url = "https://github.com/ivmai/libatomic_ops/releases/download/v7.6.2/libatomic_ops-7.6.2.tar.gz"
    , libVersion = [7,6,2]
    }
in

let gc =
  prelude.dep //
    { libName = "gc"
    , dir = "gc-7.6.4"
    , url = "https://github.com/ivmai/bdwgc/releases/download/v7.6.4/gc-7.6.4.tar.gz"
    , libVersion = [7,6,4]
    , libDeps = prelude.mapPlainDeps [ "atomic-ops" ]
    }
in

let divideConquer =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquer", unpackDir = "atscntrb-bucs320-divideconquer" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ] }
in

let divideConquerPar =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquerpar", unpackDir = "atscntrb-bucs320-divideconquerpar" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-divideconquer" ] }
in

let fworkshop =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-fworkshop", unpackDir = "atscntrb-hx-fworkshop" }
in

let intinf =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-intinf", unpackDir = "atscntrb-hx-intinf" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-libgmp" ] }
in

let threadkit =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-threadkit", unpackDir = "atscntrb-hx-threadkit" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-libgmp" ] }
in

let pkgset =
  [ divideConquer [1,0,5]
  , divideConquerPar [1,0,9]
  , fworkshop [1,0,2]
  , intinf [1,0,0]
  , threadkit [1,0,3]
  , gmp [1,0,4]
  , atomicOps
  , gc
  , fastArithmetic [0,3,3,0]
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/pkg.dhall [0,4,6]
  , https://raw.githubusercontent.com/vmchale/hs-bind/master/pkg.dhall
  , https://raw.githubusercontent.com/vmchale/nproc-ats/master/pkg.dhall [0,1,5]
  , https://raw.githubusercontent.com/vmchale/either/master/pkg.dhall [0,2,1]
  , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall [0,2,2]
  ]

in pkgset
