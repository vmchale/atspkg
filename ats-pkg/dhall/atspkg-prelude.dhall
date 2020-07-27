{- Dhall prelude functions -}
let concatMapSep =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840

let map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let ATSConstraint =
      { lower : Optional (List Natural), upper : Optional (List Natural) }

let LibDep = { _1 : Text, _2 : ATSConstraint }

let LinkType = { _1 : Text, _2 : Text }

let ForeignCabal =
      { projectFile : Optional Text, cabalFile : Text, objectFile : Text }

let TargetPair = { hs : Text, ats : Text, cpphs : Bool }

let CCompiler = < CompCert | Clang | GCC | ICC | Pgi | CC >

let Bin =
      { src : Text
      , target : Text
      , libs : List Text
      , hsDeps : List ForeignCabal
      , hs2ats : List TargetPair
      , gcBin : Bool
      , extras : List Text
      }

let Lib =
      { name : Text
      , src : List Text
      , libTarget : Text
      , libs : List Text
      , includes : List Text
      , hsDeps : List ForeignCabal
      , links : List LinkType
      , hs2ats : List TargetPair
      , extras : List Text
      , static : Bool
      }

let Src =
      { atsSrc : Text
      , cTarget : Text
      , atsGen : List TargetPair
      , extras : List Text
      }

let Script = { configure : Optional Text, build : Text, install : Text }

let Solver = < PatsSolve | Z3 | Ignore >

let Debian =
      { package : Text
      , version : List Natural
      , maintainer : Text
      , description : Text
      , target : Text
      , manpage : Optional Text
      , binaries : List Text
      , libraries : List Text
      , headers : List Text
      , license : Optional Text
      , changelog : Optional Text
      }

let AtsPkg =
      { bin : List Bin
      , test : List Bin
      , bench : List Bin
      , libraries : List Lib
      , man : Optional Text
      , completions : Optional Text
      , version : List Natural
      , compiler : List Natural
      , dependencies : List LibDep
      , clib : List LibDep
      , buildDeps : List LibDep
      , ccompiler : Text
      , cflags : List Text
      , atsFlags : List Text
      , atsSource : List Src
      , dynLink : Bool
      , extSolve : Solver
      , debPkg : Optional Debian
      , atsLib : Bool
      }

let autoconfScript =
      λ(x : { dir : Text, target : Optional Text }) →
          { configure = Some "./configure --prefix=${x.dir}"
          , build = "make -j6"
          , install = "make install"
          }
        : Script

let src =
      λ(x : { atsSrc : Text, cTarget : Text }) →
        { atsSrc = x.atsSrc
        , cTarget = x.cTarget
        , atsGen = [] : List TargetPair
        , extras = [] : List Text
        }

let mapSrc =
      λ(x : List { atsSrc : Text, cTarget : Text }) →
        map { atsSrc : Text, cTarget : Text } Src src x

let patsHome = ".atspkg/contrib"

let showVersion = λ(x : List Natural) → concatMapSep "." Natural Natural/show x

let none = None (List Natural)

let plainDeps = λ(x : Text) → { _1 = x, _2 = { lower = none, upper = none } }

let eqDeps =
      λ(x : { name : Text, version : List Natural }) →
        { _1 = x.name, _2 = { lower = Some x.version, upper = Some x.version } }

let lowerDeps =
      λ(x : { name : Text, version : List Natural }) →
        { _1 = x.name, _2 = { lower = Some x.version, upper = none } }

let upperDeps =
      λ(x : { name : Text, version : List Natural }) →
        { _1 = x.name, _2 = { lower = none, upper = Some x.version } }

let mapPlainDeps = λ(x : List Text) → map Text LibDep plainDeps x

let dep =
      { dir = patsHome
      , libVersion = [ 0, 1, 0 ]
      , libDeps = [] : List LibDep
      , libBldDeps = [] : List LibDep
      , libCDeps = [] : List LibDep
      , description = None Text
      , script = [] : List Text
      }

let common =
      { libs = [] : List Text
      , hsDeps = [] : List ForeignCabal
      , hs2ats = [] : List TargetPair
      , extras = [] : List Text
      }

let bin = common ⫽ { gcBin = False }

let lib =
        common
      ⫽ { links = [] : List { _1 : Text, _2 : Text }
        , includes = [] : List Text
        , static = False
        }

let staticLib = lib ⫽ { static = True }

let solver = Solver.PatsSolve

let ignore = Solver.Ignore

let default =
        { bin = [] : List Bin
        , test = [] : List Bin
        , bench = [] : List Bin
        , libraries = [] : List Lib
        , man = None Text
        , completions = None Text
        , version = [ 0, 3, 13 ]
        , compiler = [ 0, 3, 13 ]
        , dependencies = [] : List LibDep
        , clib = [] : List LibDep
        , buildDeps = [] : List LibDep
        , ccompiler = "gcc"
        , cflags = [ "-O2" ]
        , atsFlags = [] : List Text
        , atsSource = [] : List Src
        , dynLink = True
        , extSolve = solver
        , debPkg = None Debian
        , atsLib = True
        }
      : AtsPkg

let debian =
      λ(project : Text) →
        { package = project
        , target = "target/${project}.deb"
        , manpage = None Text
        , binaries = [] : List Text
        , libraries = [] : List Text
        , headers = [] : List Text
        , license = None Text
        , changelog = None Text
        }

let makePkg =
      λ(rec : { x : List Natural, name : Text, githubUsername : Text }) →
          dep
        ⫽ { libName = rec.name
          , dir = "${patsHome}"
          , url =
              "https://github.com/${rec.githubUsername}/${rec.name}/archive/${showVersion
                                                                                rec.x}.tar.gz"
          , libVersion = rec.x
          }

let makeNpmPkg =
      λ(rec : { x : List Natural, name : Text, unpackDir : Text }) →
          dep
        ⫽ { libName = rec.name
          , dir = "${patsHome}/${rec.unpackDir}"
          , url =
              "https://registry.npmjs.org/${rec.unpackDir}/-/${rec.unpackDir}-${showVersion
                                                                                  rec.x}.tgz"
          , libVersion = rec.x
          }

let makeHsPkg =
      λ(rec : { x : List Natural, name : Text }) →
          dep
        ⫽ { libName = rec.name
          , dir = "${patsHome}"
          , url =
              "https://hackage.haskell.org/package/${rec.name}-${showVersion
                                                                   rec.x}/${rec.name}-${showVersion
                                                                                          rec.x}.tar.gz"
          , libVersion = rec.x
          }

let makePkgDescr =
      λ ( x
        : { x : List Natural
          , name : Text
          , githubUsername : Text
          , description : Text
          }
        ) →
          makePkg { x = x.x, name = x.name, githubUsername = x.githubUsername }
        ⫽ { description = Some x.description }

let cabalDir = "dist-newstyle/lib"

let emptySrc = [] : List Src

let emptyBin = [] : List Bin

let emptyLib = [] : List Lib

let mkDeb = λ(deb : Debian) → Some deb

let noPrelude =
      [ "-D_ATS_CCOMP_PRELUDE_NONE_"
      , "-D_ATS_CCOMP_EXCEPTION_NONE_"
      , "-D_ATS_CCOMP_RUNTIME_NONE_"
      ]

let atsProject = "target"

let gcc = CCompiler.GCC

let clang = CCompiler.Clang

let compCert = CCompiler.CompCert

let icc = CCompiler.ICC

let pgi = CCompiler.Pgi

let cc = CCompiler.CC

let printCompiler =
      λ(cc : CCompiler) →
        merge
          { CompCert = "ccomp"
          , Clang = "clang"
          , GCC = "gcc"
          , ICC = "icc"
          , Pgi = "pgcc"
          , CC = "cc"
          }
          cc

let ccFlags =
      λ(cc : CCompiler) →
        merge
          { CompCert = [ "-O2", "-fstruct-passing" ]
          , Clang = [ "-O2", "-mtune=native", "-flto" ]
          , GCC = [ "-O2", "-mtune=native", "-flto" ]
          , ICC =
            [ "-O2", "-mtune=native", "-flto", "-D__PURE_INTEL_C99_HEADERS__" ]
          , CC = [ "-O2" ]
          , Pgi = [ "-O2", "-mtune=native" ]
          }
          cc

let iccFlags = [ "-D__PURE_INTEL_C99_HEADERS__" ]

let compilerMod =
      λ(cc : CCompiler) →
      λ(x : AtsPkg) →
        x ⫽ { ccompiler = printCompiler cc, cflags = x.cflags # ccFlags cc }

in  { mkDeb
    , emptySrc
    , emptyBin
    , emptyLib
    , showVersion
    , makePkg
    , bin
    , lib
    , dep
    , staticLib
    , default
    , plainDeps
    , lowerDeps
    , upperDeps
    , eqDeps
    , mapPlainDeps
    , src
    , mapSrc
    , makePkgDescr
    , makeHsPkg
    , makeNpmPkg
    , patsHome
    , cabalDir
    , solver
    , ignore
    , debian
    , noPrelude
    , atsProject
    , gcc
    , clang
    , compCert
    , icc
    , pgi
    , cc
    , printCompiler
    , ccFlags
    , iccFlags
    , autoconfScript
    , compilerMod
    }
