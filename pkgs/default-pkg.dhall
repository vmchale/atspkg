let dep = λ(x : List Integer) →
  { dir = ".atspkg/contrib"
  , libVersion = x
    : List Integer
  , libDeps = []
    : List Text
  }

in dep [0,1,0]
