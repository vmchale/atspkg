let pkg
  = { bin = []
      : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , test = []
      : List { src : Text, target : Text, libs : List Text, gc : Bool }
    , man = ([] : Optional Text)
    , version = [0,3,9]
    }

in pkg
