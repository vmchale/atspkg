{- Dhall prelude imports -}
let map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let concat =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/concat sha256:54e43278be13276e03bd1afa89e562e94a0a006377ebea7db14c7562b0de292b

let prelude =
      https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall sha256:69bdde38a8cc01c91a1808ca3f45c29fe754c9ac96e91e6abd785508466399b4

let mapDir =
      λ(rec : { dir : Text, xs : List Text }) →
        map Text Text (λ(x : Text) → "${rec.dir}/DATS/${x}.dats") rec.xs

let mapPre = λ(xs : List Text) → mapDir { dir = "prelude", xs }

let mapC = λ(xs : List Text) → mapDir { dir = "libats/libc", xs }

let mapML = λ(xs : List Text) → mapDir { dir = "libats/ML", xs }

let atslib =
      λ(compilerVersion : List Natural) →
      λ(libVersion : List Natural) →
          prelude.default
        ⫽ { libraries =
            [   prelude.staticLib
              ⫽ { libTarget = "target/libatslib.a"
                , name = "atslib"
                , src =
                    concat
                      Text
                      [ mapPre
                          [ "bool"
                          , "integer"
                          , "basics"
                          , "pointer"
                          , "integer_long"
                          , "integer_short"
                          , "integer_size"
                          , "char"
                          , "float"
                          , "string"
                          , "strptr"
                          , "integer_ptr"
                          , "integer_fixed"
                          , "filebas"
                          ]
                      , mapC
                          [ "math"
                          , "float"
                          , "errno"
                          , "fcntl"
                          , "dirent"
                          , "stdio"
                          , "stdlib"
                          , "string"
                          , "strings"
                          , "time"
                          , "unistd"
                          ]
                      , mapML
                          [ "list0"
                          , "option0"
                          , "array0"
                          , "matrix0"
                          , "string"
                          , "strarr"
                          , "gvalue"
                          , "dynarray"
                          , "hashtblref"
                          , "filebas"
                          , "filebas_dirent"
                          ]
                      ]
                , includes = [] : List Text
                }
            ]
          , cflags = [ "-fPIC" ]
          , compiler = compilerVersion
          , version = libVersion
          }

in  atslib [ 0, 3, 13 ] [ 0, 3, 13 ]
