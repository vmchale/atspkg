datavtype null =
  | null

datavtype token =
  | string_tok of string
  | int_tok of int
  | eq_tok
  | pound_tok
  | float_tok of float
  | bool_tok of bool

datavtype error_state =
  | okay
  | error_state of string

vtypedef cstream = stream_vt(char)
vtypedef tstream = stream_vt(token)

datavtype either(a : t@ype, b : t@ype+) =
  | left of a
  | right of b

vtypedef parser(a : vt@ype+) = @{ modify = cstream -<lincloptr1> (cstream, a) }
