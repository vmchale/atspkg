-- let Constraint = < LessThanEq : { _1 : List Integer } | Eq : { _1 : List Integer } | None : {} >
-- in
let dep = λ(x : List Integer) →
  { dir = ".atspkg/contrib"
  , libVersion = x
    : List Integer
  , libDeps = []
    : List (Text, Text)
  }

in dep [0,1,0]
