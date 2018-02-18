let ForeignCabal = { projectFile : Optional Text, cabalFile : Text, objectFile : Text }
in
let TargetPair = { hs : Text, ats : Text, cpphs : Bool }
in

let lib =
  { libs = ([] : List Text)
  , includes = ([] : List Text)
  , hsDeps = ([] : List ForeignCabal)
  , hs2ats = ([] : List TargetPair)
  , cSources = ([] : List Text)
  , extras = ([] : List Text)
  , static = True
  }

in lib
