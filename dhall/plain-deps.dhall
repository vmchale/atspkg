let T = Optional (List Integer)
in
let plainDeps = λ(x : Text) → { _1 = x, _2 = { lower : ([] : T), upper ([] : T) } }

in plainDeps
