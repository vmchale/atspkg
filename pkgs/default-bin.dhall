let bin =
  { src = "src/project.dats"
  , target = "target/project" 
  , libs = ([] : List Text)
  , hsDeps = ([] : List Text)
  , hs2ats = ([] : List { src : Text, target : Text }))
  , gc = False
  }

in bin
