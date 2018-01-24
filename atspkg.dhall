let pkg
  = { bin = []
      : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , dependencies = []
      : List { libName : Text, dir : Text, url : Text }
    , test = []
      : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , man = ([] : Optional Text)
    , version = [0,3,9]
    }

in pkg
