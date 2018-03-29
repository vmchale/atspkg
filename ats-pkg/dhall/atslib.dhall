{- Dhall prelude imports -}
let map = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/map
in
let concat = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/concat
in

{- Helper functions -}
let mapDir =
  λ(rec : {dir : Text, xs : List Text }) →
    map Text Text
      (λ(x : Text) → "${rec.dir}/DATS/${x}.dats")
      rec.xs
in

let mapUtil =
  λ(xs : List Text) →
    map Text Text
      (λ(x : Text) → "utils/atscc/DATS/atscc_${x}.dats")
      xs
in

let mapPre =
  λ(xs : List Text) →
    mapDir { dir = "prelude", xs = xs }
in

let mapC =
  λ(xs : List Text) →
    mapDir { dir = "libats/libc", xs = xs }
in

let mapML =
  λ(xs : List Text) →
    mapDir { dir = "libats/ML", xs = xs }
in

{- ATSPackage parts -}
let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall

in prelude.default ⫽
  { libraries =
    [
      prelude.staticLib ⫽
      { libTarget = "lib/ats2-postiats-0.3.9/ccomp/atslib/lib/libatslib.a"
      , name = "atslib"
      , src =
        concat Text
          [ mapPre [ "bool", "integer", "basics", "pointer", "integer_long", "integer_short", "integer_size", "char", "float", "string", "strptr", "integer_ptr", "integer_fixed", "filebas" ]
          , mapC [ "math", "float", "errno", "fcntl", "dirent", "stdio", "stdlib", "string", "strings", "time", "unistd" ]
          , mapML [ "list0", "option0", "array0", "matrix0", "string", "strarr", "gvalue", "dynarray", "hashtblref", "filebas", "filebas_dirent" ]
          ]
      , includes = ([] : List Text)
      }
    , prelude.staticLib ⫽
      { libTarget = "lib/ats2-postiats-0.3.9/ccomp/atslib/lib/libatsopt.a"
      , name = "atsopt"
      , src = mapUtil [ "main", "print", "util" ]
      }
    ]
  , cflags = [ "-fPIC" ]
  , compiler = [0,3,10]
  }
