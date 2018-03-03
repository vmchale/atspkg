#define ATS_MAINATSFLAG 1

#include "share/atspre_staload.hats"

staload "libats/libc/SATS/math.sats"
staload UN = "prelude/SATS/unsafe.sats"

fun exp {n : nat} .<n>. (x : int, n : int(n)) :<> int =
  case+ x of
    | 0 => 0
    | x => 
      begin
        if n > 0 then
          let
            val n2 = half(n)
            val i2 = n % 2
          in
            if i2 = 0 then
              exp(x * x, n2)
            else
              x * exp(x * x, n2)
          end
        else
          1
      end

castfn lemma_bounded(i: int) : [n:nat] int(n)
  = $UN.cast(i)

fun sqrt_bad(k : intGt(0)) : [ m : nat ] int(m) =
  let
    var pre_bound: int = g0float2int(sqrt_float(g0int2float_int_float(k)))
    var bound: [m:nat] int(m) = lemma_bounded(pre_bound)
  in
    bound
  end

fn is_prime(k : intGt(0)) : bool =
  case+ k of
    | 1 => false
    | k => 
      begin
        let
          fun loop {n : nat}{m : nat} .<max(0,m-n)>. (i : int(n), bound : int(m)) :<> bool =
            if i < bound then
              if k % i = 0 then
                false
              else
                loop(i + 1, bound)
            else
              if i = bound then
                if k % i = 0 then
                  false
                else
                  true
              else
                true
        in
          loop(2, sqrt_bad(k))
        end
      end
