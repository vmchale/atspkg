{- Import the atspkg prelude -}
let prelude = http://hackage.haskell.org/package/ats-pkg/src/dhall/atspkg-prelude.dhall
in

-- https://github.com/facebook/zstd
{- Packages -}
let makeGnuPkg =
  λ(rec : { version : List Natural, name: Text}) →
    prelude.dep ⫽
      { libName = rec.name
      , dir = "${rec.name}-${prelude.showVersion rec.version}"
      , url = "https://mirrors.ocf.berkeley.edu/gnu/lib${rec.name}/lib${rec.name}-${prelude.showVersion rec.version}.tar.xz"
      , libVersion = rec.version
      }
in

let atsIncludes =
  λ(x : List Natural) →
    prelude.dep ⫽
      { libName = "ats-includes"
      , dir = "${prelude.patsHome}/ats-includes-${prelude.showVersion x}"
      , url = "https://github.com/vmchale/atspkg/releases/download/compiler/ATS2-Postiats-${prelude.showVersion x}.tar.gz"
      , libVersion = x
      }
in

let curl =
  λ(x : List Natural) →
    prelude.dep ⫽
      { libName = "curl"
      , dir = "curl-${prelude.showVersion x}"
      , url = "https://curl.haxx.se/download/curl-${prelude.showVersion x}.tar.xz"
      , libVersion = x
      , libCDeps = prelude.mapPlainDeps [ "ssl" ]
      }
in

let unistring =
  λ(x : List Natural) →
    makeGnuPkg { version = x, name = "unistring" }
in

-- http://phash.org/releases/pHash-0.9.6.tar.gz
-- http://cimg.eu/files/CImg_latest.zip
-- https://ffmpeg.org/releases/ffmpeg-4.0.2.tar.bz2
let fastArithmetic = https://hackage.haskell.org/package/fast-arithmetic/src/pkg.dhall
in

let gmp =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-libgmp", unpackDir = "atscntrb-hx-libgmp" }
in

let parcomb =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-parcomb", unpackDir = "atscntrb-hx-parcomb" }
in

let atomicOps =
  λ(v : List Natural) →
    prelude.dep ⫽
      { libName = "atomic-ops"
      , dir = "atomic-ops-${prelude.showVersion v}"
      , url = "https://github.com/ivmai/libatomic_ops/releases/download/v${prelude.showVersion v}/libatomic_ops-${prelude.showVersion v}.tar.gz"
      , libVersion = v
      }
in

let gc =
  λ(v : List Natural) →
    prelude.dep ⫽
      { libName = "gc"
      , dir = "gc-${prelude.showVersion v}"
      , url = "https://github.com/ivmai/bdwgc/releases/download/v${prelude.showVersion v}/gc-${prelude.showVersion v}.tar.gz"
      , libVersion = v
      , libDeps = prelude.mapPlainDeps [ "atomic-ops" ]
      }
in

let divideConquer =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquer", unpackDir = "atscntrb-bucs320-divideconquer" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ] }
in

let divideConquerPar =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquerpar", unpackDir = "atscntrb-bucs320-divideconquerpar" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-divideconquer" ] }
in

let fworkshop =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-fworkshop", unpackDir = "atscntrb-hx-fworkshop" }
in

let intinf =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-intinf", unpackDir = "atscntrb-hx-intinf" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-libgmp" ] }
in

let threadkit =
  λ(x : List Natural) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-threadkit", unpackDir = "atscntrb-hx-threadkit" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-libgmp" ] }
in

let pkgset =
  [ divideConquer [1,0,5]
  , divideConquerPar [1,0,9]
  , fworkshop [1,0,2]
  , intinf [1,0,8]
  , threadkit [1,0,3]
  , gmp [1,0,1]
  , atomicOps [7,6,10]
  , gc [8,0,4]
  , gc [7,6,8]
  , gc [7,6,10]
  , gc [7,6,12]
  , fastArithmetic [0,6,4,2]
  , unistring [0,9,10]
  , atsIncludes [0,3,12]
  , atsIncludes [0,3,13]
  , curl [7,60,0]
  , parcomb [1,0,7]
  , https://raw.githubusercontent.com/vmchale/ats-bench/master/pkg.dhall [0,3,3]
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/pkg.dhall [0,4,8]
  , https://raw.githubusercontent.com/vmchale/hs-bind/master/pkg.dhall [0,4,3]
  , https://raw.githubusercontent.com/vmchale/nproc-ats/master/pkg.dhall [0,1,8]
  , https://raw.githubusercontent.com/vmchale/either/master/pkg.dhall [0,2,2]
  , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall [0,2,5]
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall [0,2,3]
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall [0,4,0]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall [0,1,0]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall [0,3,3]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall [0,4,0]
  , https://raw.githubusercontent.com/vmchale/monads/master/pkg.dhall [0,1,0]
  ]

in pkgset
