#include "prelude/DATS/integer.dats"

fnx fib { n : int | n >= 0 } .<n>. (i : int(n)) : int =
case+ i of
| _ when i - 2 >= 0 => ( fib(i-1) + fib(i-2) )
| _ => 1
