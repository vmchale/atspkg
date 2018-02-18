let none = [] : Optional (List Integer)
in
let plainDeps = λ(x : Text) → { _1 = x, _2 = { lower = none, upper = none } }

in plainDeps
