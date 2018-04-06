{- Import the atspkg prelude -}
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall
in

{- Packages -}
let makeGnuPkg = 
  λ(rec : { version : List Integer, name: Text}) →
    prelude.dep ⫽
      { libName = rec.name
      , dir = "${rec.name}-${prelude.showVersion rec.version}"
      , url = "https://mirrors.ocf.berkeley.edu/gnu/lib${rec.name}/lib${rec.name}-${prelude.showVersion rec.version}.tar.xz"
      , libVersion = rec.version
      }
in

let unistring =
  makeGnuPkg { version = [0,9,9], name = "unistring" }
in

let ssl =
  prelude.dep ⫽
    { libName = "openssl"
    , dir = "openssl-1.1.0h"
    , url = "https://www.openssl.org/source/openssl-1.1.0h.tar.gz"
    , libVersion = [1,1,0]
    }
in

let ssh2 = 
  λ(x : List Integer) →
    prelude.dep ⫽
      { libName = "ssh2"
      , dir = "ssh2-${prelude.showVersion x}"
      , url = "https://www.libssh2.org/download/libssh2-${prelude.showVersion x}.tar.gz"
      , libVersion = x
      }
in

let curl = 
  λ(x : List Integer) →
    prelude.dep ⫽
      { libName = "curl"
      , dir = "curl-${prelude.showVersion x}"
      , url = "https://curl.haxx.se/download/curl-${prelude.showVersion x}.tar.gz"
      , libVersion = x
      -- , libDeps = prelude.mapPlainDeps [ "ssh2" ]
      }
in

let xzUtils =
  λ(x : List Integer) →
    prelude.dep ⫽
      { libName = "xz-utils"
      , dir = "xz-utils-${prelude.showVersion x}"
      , url = "https://tukaani.org/xz/xz-${prelude.showVersion x}.tar.gz"
      , libVersion = x
      }
in

let fastArithmetic =
  λ(x : List Integer) →
    prelude.makeHsPkg { x = x, name = "fast-arithmetic" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-intinf" ], description = [ "Number theory & combinatorics library written in ATS" ] : Optional Text }
in

let gmp =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-libgmp", unpackDir = "atscntrb-libgmp" }
      ⫽ { url = "https://registry.npmjs.org/atscntrb-hx-libgmp/-/atscntrb-hx-libgmp-${prelude.showVersion x}.tgz" }
in

let atomicOps =
  prelude.dep ⫽
    { libName = "atomic-ops"
    , dir = "atomic-ops-7.6.2"
    , url = "https://github.com/ivmai/libatomic_ops/releases/download/v7.6.2/libatomic_ops-7.6.2.tar.gz"
    , libVersion = [7,6,2]
    }
in

let git =
  λ(v : List Integer) →
    prelude.dep ⫽
      { libName = "git2"
      , dir = "libgit2-${prelude.showVersion v}"
      , url = "https://github.com/libgit2/libgit2/archive/v${prelude.showVersion v}.tar.gz"
      , libVersion = v
      , libDeps = prelude.mapPlainDeps [ "curl" ] -- , "openssl" ]
      }
in

let gc =
  prelude.dep ⫽
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
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ] }
in

let divideConquerPar =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-divideconquerpar", unpackDir = "atscntrb-bucs320-divideconquerpar" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-divideconquer" ] }
in

let fworkshop =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-fworkshop", unpackDir = "atscntrb-hx-fworkshop" }
in

let intinf =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-intinf", unpackDir = "atscntrb-hx-intinf" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-libgmp" ] }
in

let threadkit =
  λ(x : List Integer) →
    prelude.makeNpmPkg { x = x, name = "atscntrb-hx-threadkit", unpackDir = "atscntrb-hx-threadkit" }
      ⫽ { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-libgmp" ] }
in

let pkgset =
  [ divideConquer [1,0,5]
  , divideConquerPar [1,0,9]
  , fworkshop [1,0,2]
  , intinf [1,0,8]
  , threadkit [1,0,3]
  , gmp [1,0,0]
  , atomicOps
  , gc
  , fastArithmetic [0,3,3,1]
  , unistring
  , xzUtils [5,2,3]
  , git [0,27,0]
  , curl [7,59,0]
  -- , ssh2 [1,8,0]
  -- , ssl
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/pkg.dhall [0,4,7]
  , https://raw.githubusercontent.com/vmchale/hs-bind/master/pkg.dhall [0,4,1]
  , https://raw.githubusercontent.com/vmchale/nproc-ats/master/pkg.dhall [0,1,5]
  , https://raw.githubusercontent.com/vmchale/either/master/pkg.dhall [0,2,2]
  , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall [0,2,5]
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall [0,2,2]
  ]

in pkgset
