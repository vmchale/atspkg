#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

fnx fact_boring(n: int) : int =
  case+ n of
    | 0 => 1
    | n => n * fact_boring(n-1)

(* fnx collatz{n:nat} *)
(* (n: int(n)) : int = let *)
(* fun loop{n:nat}{l:addr} .<n>. *)
(* (pf: !int @ l | n: int n, res: ptr l) : void = *)
(* if n > 1  *)

// TODO rewrite this for collatz?
fnx fact{n:nat}
(n: int (n)): int = let
fun loop{n:nat}{l:addr} .<n>.
(pf: !int @ l | n: int n, res: ptr l): void =
if n > 0 then let
val () = !res := n * !res in loop (pf | n-1, res)
end // end of [if]
// end of [loop]
var res: int with pf = 1
val () = loop (pf | n, addr@res) // addr@res: the pointer to res
in
res
end // end of [fact]

implement main0 () =
  let val x = fact(30) in
  println!(tostring_int(x)) end
