let pkg
  = { bin = []
      : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , test = []
      : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , man = ([] : Optional Text)
    , version = [0,3,9]
    , compiler = [0,3,9]
    , dependencies = []
      : List { libName : Text, dir : Text, url : Text }
    , clib = []
      : List { libName : Text, dir : Text, url : Text }
    , ccompiler = "gcc"
    , cflags = [ "-O2", "-flto" ]
    }

in pkg
