let ForeignCabal = { projectFile : Optional Text, cabalFile : Text, objectFile : Text }
in
let TargetPair = { hs : Text, ats : Text, cpphs : Bool }
in
let Bin = { src : Text, target : Text, libs : List Text, hsDeps : List ForeignCabal , hs2ats : List TargetPair, gcBin : Bool, cSources : List Text, extras : List Text }
in
let Lib = { name : Text, src : List Text, libTarget : Text, libs : List Text, includes : List Text, hsDeps : List ForeignCabal, hs2ats : List TargetPair, cSources : List Text, extras : List Text, static : Bool }
in

let pkg
  = { bin = []
      : List Bin
    , test = []
      : List Bin
    , libraries = []
      : List Lib
    , man = ([] : Optional Text)
    , version = [0,3,9]
    , compiler = [0,3,9]
    , dependencies = []
      : List Text
    , clib = []
      : List Text
    , buildDeps = []
      : List Text
    , ccompiler = "gcc"
    , cflags = [ "-O2" ]
    , atsSource = ([] : List Text)
    , cDir = "cbits"
    }

in pkg
