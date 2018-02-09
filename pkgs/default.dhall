let pkg
  = { bin = []
    , test = []
    , man = ([] : Optional Text)
    , version = [0,3,9]
    , compiler = [0,3,9]
    , dependencies = []
    , clib = []
    , ccompiler = "gcc"
    , cflags = [ "-O2", "-flto" ]
    , atsSource = []
    , cDir = "cbits"
    }

in pkg
