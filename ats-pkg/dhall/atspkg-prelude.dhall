{- Dhall prelude functions -}
let concatMapSep = https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/Text/concatMapSep sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840
in
let map = https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
in

{- Types for export and supporting functions -}
let ATSConstraint = { lower : Optional (List Natural), upper : Optional (List Natural) }
in

let LibDep = { _1 : Text, _2 : ATSConstraint }
in

let LinkType = { _1 : Text, _2 : Text }
in

let ForeignCabal = { projectFile : Optional Text, cabalFile : Text, objectFile : Text }
in

let TargetPair = { hs : Text, ats : Text, cpphs : Bool }
in

let CCompiler = < CompCert | Clang | GCC | ICC | CC >
in

let Bin =
  { src : Text
  , target : Text
  , libs : List Text
  , hsDeps : List ForeignCabal
  , hs2ats : List TargetPair
  , gcBin : Bool
  , extras : List Text
  }
in

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
in

let Src =
  { atsSrc : Text
  , cTarget : Text
  , atsGen : List TargetPair
  , extras : List Text
  }
in

let Script = { configure : Optional Text, build : Text, install : Text }
in

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
in

let script =
  λ(x : {dir : Text, target : Optional Text}) →
    { configure = Some "./configure --prefix=${x.dir}", build = "make -j6", install = "make install" } : Script
in

let src =
  λ(x : { atsSrc : Text, cTarget : Text }) →
    { atsSrc = x.atsSrc
    , cTarget = x.cTarget
    , atsGen = []
      : List TargetPair
    , extras = []
      : List Text
    }
in

let iccFlags =
    [ "-D__PURE_INTEL_C99_HEADERS__" ]
in

let mapSrc = λ(x : List { atsSrc : Text, cTarget : Text}) → map { atsSrc : Text, cTarget : Text } Src src x
in

{- Helper functions -}
let patsHome =
  ".atspkg/contrib"
in

let showVersion =
  λ(x : List Natural) → concatMapSep "." Natural Natural/show x
in

let none = None (List Natural)
in
let plainDeps = λ(x : Text) → { _1 = x, _2 = { lower = none, upper = none } }
in

let eqDeps = λ(x : { name : Text, version : List Natural }) →
    { _1 = x.name
    , _2 = { lower = Some x.version
           , upper = Some x.version
           }
    }
in

let lowerDeps = λ(x : { name : Text, version : List Natural }) →
    { _1 = x.name
    , _2 = { lower = Some x.version
           , upper = none
           }
    }
in

let upperDeps = λ(x : { name : Text, version : List Natural }) →
    { _1 = x.name
    , _2 = { lower = none
           , upper = Some x.version
           }
    }
in

let mapPlainDeps = λ(x : List Text) → map Text LibDep plainDeps x
in

{- Default configurations -}
let dep =
  { dir = patsHome
  , libVersion = [0,1,0]
  , libDeps = []
    : List LibDep
  , libBldDeps = []
    : List LibDep
  , libCDeps = []
    : List LibDep
  , description = None Text
  , script = []
    : List Text
  }
in

let common =
  { libs = ([] : List Text)
  , hsDeps = ([] : List ForeignCabal)
  , hs2ats = ([] : List TargetPair)
  , extras = ([] : List Text)
  }
in

let bin =
  common ⫽ { gcBin = False }
in

let lib =
  common ⫽
    { links = ([] : List { _1 : Text, _2 : Text })
    , includes = ([] : List Text)
    , static = False
    }
in

let staticLib =
  lib ⫽ { static = True }
in

let Solver = < PatsSolve | Z3 | Ignore >
in

let solver = Solver.PatsSolve
in

let ignore = Solver.Ignore
in

let default
  = { bin = []
      : List Bin
    , test = []
      : List Bin
    , bench = []
      : List Bin
    , libraries = []
      : List Lib
    , man = None Text
    , completions = None Text
    , version = [0,3,13]
    , compiler = [0,3,13]
    , dependencies = []
      : List LibDep
    , clib = []
      : List LibDep
    , buildDeps = []
      : List LibDep
    , ccompiler = "gcc"
    , cflags = [ "-O2" ]
    , atsFlags = []
      : List Text
    , atsSource = []
      : List Src
    , dynLink = True
    , extSolve = solver
    , debPkg = None Debian
    , atsLib = True
    }
in

let debian =
  λ(project : Text) →
  { package = project
  , target = "target/${project}.deb"
  , manpage = None Text
  , binaries = []
    : List Text
  , libraries = []
    : List Text
  , headers = []
    : List Text
  , license = None Text
  , changelog = None Text
  }
in

{- Package functions -}
let makePkg =
  λ(rec : { x : List Natural, name : Text, githubUsername : Text}) →
    dep ⫽
      { libName = rec.name
      , dir = "${patsHome}"
      , url = "https://github.com/${rec.githubUsername}/${rec.name}/archive/${showVersion rec.x}.tar.gz"
      , libVersion = rec.x
      }
in

let makeNpmPkg =
  λ(rec : { x : List Natural, name : Text, unpackDir : Text }) →
    dep ⫽
      { libName = rec.name
      , dir = "${patsHome}/${rec.unpackDir}"
      , url = "https://registry.npmjs.org/${rec.unpackDir}/-/${rec.unpackDir}-${showVersion rec.x}.tgz"
      , libVersion = rec.x
      }
in

let makeHsPkg =
  λ(rec : { x : List Natural, name : Text }) →
    dep ⫽
      { libName = rec.name
      , dir = "${patsHome}"
      , url = "https://hackage.haskell.org/package/${rec.name}-${showVersion rec.x}/${rec.name}-${showVersion rec.x}.tar.gz"
      , libVersion = rec.x
      }
in

let makePkgDescr =
  λ(x : { x : List Natural, name : Text, githubUsername : Text, description : Text }) →
    makePkg { x = x.x, name = x.name, githubUsername = x.githubUsername }
      ⫽ { description = Some (x.description) }
in

let cabalDir = "dist-newstyle/lib"
in

{- Various empty directories because Dhall no longer allows type exports -}
let emptySrc =
  [] : List Src
in

let emptyBin =
  [] : List Bin
in

let emptyLib =
  [] : List Lib
in

let mkDeb =
  λ(deb : Debian) →
    Some deb
in

let noPrelude =
  [ "-D_ATS_CCOMP_PRELUDE_NONE_", "-D_ATS_CCOMP_EXCEPTION_NONE_", "-D_ATS_CCOMP_RUNTIME_NONE_" ]
in

let atsProject = "target"
in

let gcc = CCompiler.GCC
in
let clang = CCompiler.Clang
in
let compCert = CCompiler.CompCert
in
let icc = CCompiler.ICC
in
let cc = CCompiler.CC
in

let printCompiler =
    λ(cc : CCompiler) →
        merge { CompCert = "ccomp"
              , Clang = "clang"
              , GCC = "gcc"
              , ICC = "icc"
              , CC = "cc"
              }
              cc
in

let ccFlags =
    λ(cc : CCompiler) →
        merge { CompCert = [ "-O2", "-fstruct-passing" ]
              , Clang = [ "-O2", "-mtune=native", "-flto" ]
              , GCC = [ "-O2", "-mtune=native", "-flto" ]
              , ICC = [ "-O2", "-mtune=native", "-flto", "-D__PURE_INTEL_C99_HEADERS__" ]
              , CC = [ "-O2" ]
              }
              cc
in

let iccFlags = [ "-D__PURE_INTEL_C99_HEADERS__" ]
in

{- We collect everything in a single record for convenience -}
{ mkDeb = mkDeb
, emptySrc = emptySrc
, emptyBin = emptyBin
, emptyLib = emptyLib
, showVersion = showVersion
, makePkg = makePkg
, bin = bin
, lib = lib
, dep = dep
, staticLib = staticLib
, default = default
, plainDeps = plainDeps
, lowerDeps = lowerDeps
, upperDeps = upperDeps
, eqDeps = eqDeps
, mapPlainDeps = mapPlainDeps
, src = src
, mapSrc = mapSrc
, makePkgDescr = makePkgDescr
, makeHsPkg = makeHsPkg
, makeNpmPkg = makeNpmPkg
, patsHome = patsHome
, cabalDir = cabalDir
, solver = solver
, ignore = ignore
, debian = debian
, noPrelude = noPrelude
, atsProject = atsProject
, gcc = gcc
, clang = clang
, compCert = compCert
, icc = icc
, cc = cc
, printCompiler = printCompiler
, ccFlags = ccFlags
, iccFlags = iccFlags
}
