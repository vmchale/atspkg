#include "share/atspre_staload_libats_ML.hats"
#include "libats/ML/SATS/SHARE/monad.hats"

datatype either(a: t@ype, b: t@ype+) =
  | left of a
  | right of b

fun {a:t@ype}{b:t@ype+} eq_either_either  (x : either(a, INV(b)), y : either(a, b)) : bool

fun {a:t@ype}{b:t@ype+} neq_either_either  (x : either(a, INV(b)), y : either(a, b)) : bool

overload = with eq_either_either

overload != with neq_either_either

fun lefts {a:t@ype}{b:t@ype+}{n:int} (x : list(either(a,b), n)) :
  [ m : int | m <= n ] list(a, m)

fun rights {a:t@ype}{b:t@ype+}{n:int} (x : list(either(a,b), n)) :
  [ m : int | m <= n ] list(b, m)

fun either_ {a:t@ype}{b:t@ype+}{c:t@ype} (f : a -> c, g : b -> c, x : either(a, b)) : c

fun is_left {a:t@ype}{b:t@ype+} (x : either(a, b)) : bool

fun is_right {a:t@ype}{b:t@ype+} (x : either(a, b)) : bool

fun from_right {a:t@ype}{b:t@ype+} (x : b, y : either(a, b)) : b

fun from_left {a:t@ype}{b:t@ype+} (x : a, y : either(a, b)) : a