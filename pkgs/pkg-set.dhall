{- Import the atspkg prelude -}
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall
in

{- Packages -}
let fastArithmetic =
  λ(x : List Integer) → 
    prelude.makeHsPkg { x = x, name = "fast-arithmetic" } 
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-intinf" ], description = [ "Number theory & combinatorics library written in ATS" ] : Optional Text }
in

let divideConquer =
  prelude.dep //
    { libName = "atscntrb-hx-divideconquer"
    , dir = ".atspkg/contrib/atscntrb-bucs320-divideconquer"
    , url = "https://registry.npmjs.org/atscntrb-bucs320-divideconquer/-/atscntrb-bucs320-divideconquer-1.0.5.tgz"
    , libVersion = [1,0,5]
    , libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ]
    }
in

let gmp =
  λ(v : List Integer) → 
    prelude.dep //
      { libName = "atscntrb-libgmp"
      , dir = ".atspkg/contrib/atscntrb-libgmp"
      , url = "https://registry.npmjs.org/atscntrb-libgmp/-/atscntrb-libgmp-${prelude.showVersion v}.tgz"
      , libVersion = v
      }
in

let atomicOps =
  prelude.dep //
    { libName = "atomic-ops"
    , dir = "atomic-ops-7.6.2"
    , url = "https://github.com/ivmai/libatomic_ops/releases/download/v7.6.2/libatomic_ops-7.6.2.tar.gz"
    , libVersion = [7,6,2]
    }


let gc =
  prelude.dep //
    { libName = "gc"
    , dir = "gc-7.6.4"
    , url = "https://github.com/ivmai/bdwgc/releases/download/v7.6.4/gc-7.6.4.tar.gz"
    , libVersion = [7,6,4]
    , libDeps = prelude.mapPlainDeps [ "atomic-ops" ]
    }
in

  {-
let divideConquer =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquer" }
      // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ] }
in
-}

let pkgset =
  [ divideConquer
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquerpar.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-fworkshop.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-threadkit.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-intinf.dhall [1,0,8]
  , gmp [1,0,4]
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
