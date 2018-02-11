let pkg
  = { bin = []
      : List { src : Text, target : Text, libs : List Text, hsDeps : List { projectFile : Optional Text, cabalFile : Text, objectFile : Text } , hs2ats : List { hs : Text, ats : Text, cpphs : Bool }, gcBin : Bool, cSources : List Text }
    , test = []
      : List { src : Text, target : Text, libs : List Text, hsDeps : List { projectFile : Optional Text, cabalFile : Text, objectFile : Text } , hs2ats : List { hs : Text, ats : Text, cpphs : Bool }, gcBin : Bool, cSources : List Text }
    , man = ([] : Optional Text)
    , version = [0,3,9]
    , compiler = [0,3,9]
    , dependencies = []
      : List Text
    , clib = []
      : List Text
    , ccompiler = "gcc"
    , cflags = [ "-O2" ]
    , atsSource = ([] : List Text)
    , cDir = "cbits"
    }

in pkg
