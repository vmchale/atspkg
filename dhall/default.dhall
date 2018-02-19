let Bin = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/Bin.dhall
in
let Lib = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/Lib.dhall
in
let LibDep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/LibDep.dhall
in

let pkg
  = { bin = []
      : List Bin
    , test = []
      : List Bin
    , libraries = []
      : List Lib
    , man = ([] : Optional Text)
    , completions = ([] : Optional Text)
    , version = [0,3,9]
    , compiler = [0,3,9]
    , dependencies = []
      : List LibDep
    , clib = []
      : List LibDep
    , buildDeps = []
      : List LibDep
    , ccompiler = "gcc"
    , cflags = [ "-O2" ]
    , atsSource = ([] : List Text)
    , cDir = "cbits"
    }

in pkg
