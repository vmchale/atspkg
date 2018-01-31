let bin =
  { src = "src/project.dats"
  , target = "target/project" 
  , libs = ([] : List Text)
  , hsDeps = ([] : List Text)
  , gc = False
  }

in bin
