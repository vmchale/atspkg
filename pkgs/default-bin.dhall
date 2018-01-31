let bin =
  { src = "src/project.dats"
  , target = "target/project" 
  , libs = ([] : List Text)
  , hsDeps = ([] : List Text)
  , hs2ats = ([] : List (Text, Text))
  , gc = False
  }

in bin
