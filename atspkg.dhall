let pkg : { dirs : List Text, bin : { src : Text, target : Text } }
  = { dirs = [ "target" ]
    , bin = 
      { src = "src/polyglot.dats"
      , target = "target/poly" 
      }
    }

in pkg
