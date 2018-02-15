let ForeignCabal = { projectFile : Optional Text, cabalFile : Text, objectFile : Text }
in
let TargetPair = { hs : Text, ats : Text, cpphs : Bool }
in

let bin =
  { libs = ([] : List Text)
  , hsDeps = ([] : List ForeignCabal)
  , hs2ats = ([] : List TargetPair)
  , cSources = ([] : List Text)
  , extras = ([] : List Text)
  }

in bin
