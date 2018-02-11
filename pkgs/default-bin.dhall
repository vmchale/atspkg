let bin =
  { src = "src/project.dats"
  , target = "target/project" 
  , libs = ([] : List Text)
  , hsDeps = ([] : List { projectFile : Optional Text, cabalFile : Text, objectFile : Text })
  , hs2ats = ([] : List { hs : Text, ats : Text, cpphs : Bool })
  , gcBin = False
  , cSources = ([] : List Text)
  }

in bin
