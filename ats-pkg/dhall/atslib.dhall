{- Dhall prelude imports -}
let map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map
in
let concat = https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/concat
in

{- ATSPackage parts -}
let prelude = http://hackage.haskell.org/package/ats-pkg/src/dhall/atspkg-prelude.dhall
in

{- Helper functions -}
let mapDir =
  λ(rec : {dir : Text, xs : List Text }) →
    map Text Text
      (λ(x : Text) → "${rec.dir}/DATS/${x}.dats")
      rec.xs
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

let atslib =
  λ(compilerVersion : List Natural) →
    λ(libVersion : List Natural) →
      prelude.default ⫽
        { libraries =
          [
            prelude.staticLib ⫽
            { libTarget = "target/libatslib.a"
            , name = "atslib"
            , src =
              concat Text
                [ mapPre [ "bool", "integer", "basics", "pointer", "integer_long", "integer_short", "integer_size", "char", "float", "string", "strptr", "integer_ptr", "integer_fixed", "filebas" ]
                , mapC [ "math", "float", "errno", "fcntl", "dirent", "stdio", "stdlib", "string", "strings", "time", "unistd" ]
                , mapML [ "list0", "option0", "array0", "matrix0", "string", "strarr", "gvalue", "dynarray", "hashtblref", "filebas", "filebas_dirent" ]
                ]
            , includes = ([] : List Text)
            }
          ]
        , cflags = [ "-fPIC" ]
        , compiler = compilerVersion
        , version = libVersion
        }
in

atslib [0,3,13] [0,3,13]
