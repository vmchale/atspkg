// example from the book: http://ats-lang.sourceforge.net/EXAMPLE/EFFECTIVATS/PwTP-bool-vs-prop/main.html
#include "share/atspre_staload.hats"

staload "prelude/SATS/integer.sats"

infixr (->) ->>

stadef ->> (b1: bool, b2: bool) = ~b1 || b2

dataprop fib_p(int, int) =
  | fib_p_bas0(0, 0) of ()
  | fib_p_bas1(1, 1) of ()
  | {n:nat}{ r0, r1 : int } fib_p_ind2(n + 2, r0 + r1) of (fib_p( n
                                                                , r0
                                                                ), fib_p(n + 1, r1))

stacst fib_b : (int, int) -> bool

extern
praxi fib_b_bas0() : [fib_b(0,0)] unit_p

extern
praxi fib_b_bas1() : [fib_b(1,1)] unit_p

extern
praxi fib_b_ind2 {n:nat}{ r0, r1 : int } :
  [fib_b(n,r0) && fib_b(n+1,r1) ->> fib_b(n+2,r0+r1)] unit_p

fun f_fib_p {n:nat}(n : int(n)) : [r:int] (fib_p(n, r) | int(r)) =
  let
    fun loop { i : nat | i < n }{ r0, r1 : int }( pf0 : fib_p(i, r0)
                                                 , pf1 : fib_p(i+1, r1)
                                                  | i : int(i), r0 : int(r0), r1 : int(r1)) :
      [r:int] (fib_p(n, r) | int(r)) =
      if i + 1 < n then
        loop(pf1, fib_p_ind2(pf0, pf1) | i + 1, r1, r0 + r1)
      else
        (pf1 | r1)
    
    prval pf0 = fib_p_bas0()
    prval pf1 = fib_p_bas1()
  in
    if n >= 1 then
      loop(pf0, pf1 | 0, 0, 1)
    else
      (pf0 | 0)
  end

implement main0 () =
  let
    val (_ | i) = f_fib_p(40)
  in
    println!(i)
  end

