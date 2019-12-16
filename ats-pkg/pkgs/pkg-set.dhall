{- Import the atspkg prelude -}
let prelude = ../dhall/atspkg-prelude.dhall sha256:33e41e509b6cfd0b075d1a8a5210ddfd1919372f9d972c2da783c6187d2298ba
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
let fastArithmetic = https://raw.githubusercontent.com/vmchale/hs-ats/master/fast-arithmetic/pkg.dhall sha256:97c8c051e9a7bc7875c38225a32a2af99518627f7cd52c0fccec5678cc8b019a
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
  , https://raw.githubusercontent.com/vmchale/ats-bench/master/pkg.dhall sha256:d90bbb498e50e8083d1ba278131cdd4a87ce9db57e1887e1767ca3f032c085bc [0,3,3]
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/pkg.dhall sha256:75bd762aa44b3f638905ef683fe2359f8010e096db3636b783b364ac6fa86a71 [0,4,8]
  , https://raw.githubusercontent.com/vmchale/hs-bind/master/pkg.dhall sha256:a47f28f82fbffdf66af6189042ab1c68fbe70a559297516264621ea101b2e21f [0,4,3]
  , https://raw.githubusercontent.com/vmchale/nproc-ats/master/pkg.dhall sha256:3d17df178c3e24b3e680415be23cbcce8eb08f5f91cd95fdecd5f36b037c8848 [0,1,8]
  , https://raw.githubusercontent.com/vmchale/either/master/pkg.dhall sha256:f7d40b3c80f00c30b638e96a0b8e430277bde93f212ad702ac0394d5de0a7ace [0,2,5]
  , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall sha256:f7e2459d5577f84064fbb4a7cad08feefc38cfb9a272a8aac152f626f798c223 [0,2,5]
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall sha256:df389585dd2061f8bcea14db9e6598df0ef8f10a163d88bb8e178f8276f7a96c [0,2,3]
  , https://raw.githubusercontent.com/vmchale/specats/master/pkg.dhall sha256:df389585dd2061f8bcea14db9e6598df0ef8f10a163d88bb8e178f8276f7a96c [0,4,0]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall sha256:8279a13903d45b5ef1d75a2123f289a09299bc84e769f2d427c10c62158a79a4 [0,1,0]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall sha256:8279a13903d45b5ef1d75a2123f289a09299bc84e769f2d427c10c62158a79a4 [0,3,3]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall sha256:8279a13903d45b5ef1d75a2123f289a09299bc84e769f2d427c10c62158a79a4 [0,4,0]
  , https://raw.githubusercontent.com/vmchale/edit-distance/master/pkg.dhall sha256:8279a13903d45b5ef1d75a2123f289a09299bc84e769f2d427c10c62158a79a4 [0,5,0]
  , https://raw.githubusercontent.com/vmchale/monads/master/pkg.dhall sha256:194d942030ad71d3501978ac3ba80ef3be056430e1561361ec6376f73c770e15 [0,1,0]
  , https://raw.githubusercontent.com/vmchale/monads/master/pkg.dhall sha256:194d942030ad71d3501978ac3ba80ef3be056430e1561361ec6376f73c770e15 [0,2,0]
  , https://raw.githubusercontent.com/vmchale/recursion/master/pkg.dhall sha256:ccfcfcfdd15c7c025ce88df6f3c515c24aca3b0c390eb3eccb5a9155d28f28e1 [0,1,0]
  , https://raw.githubusercontent.com/vmchale/stack/master/pkg.dhall sha256:a02bc322751934a2e61ba7217f9ec4431c1615493296414cab10c4237addd011 [0,1,0]
  ]

in pkgset
