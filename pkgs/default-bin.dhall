let bin =
  { src = "src/project.dats"
  , target = "target/project" 
  , libs = ([] : List Text)
  , hsDeps = ([] : List { cabalFile : Text, objectFile : Text })
  , hs2ats = ([] : List { hs : Text, ats : Text })
  , gcBin = False
  }

in bin
