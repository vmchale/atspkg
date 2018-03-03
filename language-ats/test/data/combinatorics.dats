#define ATS_MAINATSFLAG 1

#include "share/atspre_staload.hats"

staload "contrib/atscntrb-hx-intinf/SATS/intinf_t.sats"
staload "libats/libc/SATS/math.sats"
staload "contrib/atscntrb-hx-intinf/SATS/intinf.sats"
staload UN = "prelude/SATS/unsafe.sats"

fnx fact {n : nat} .<n>. (k : int(n)) : [ n : nat | n > 0 ] intinf(n) =
  case+ k of
    | 0 => int2intinf(1)
    | 1 => int2intinf(1)
    | k =>> $UN.cast(fact(k - 1) * k)

// double factorial http://mathworld.wolfram.com/DoubleFactorial.html
fnx dfact {n : nat} .<n>. (k : int(n)) : Intinf =
  case+ k of
    | 0 => int2intinf(1)
    | 1 => int2intinf(1)
    | k =>> k * dfact(k - 2)

// Number of permutations on n objects using k at a time.
fn permutatsions {n : nat}{ k : nat | k <= n } (n : int(n), k : int(k)) : Intinf =
  ndiv(fact(n), fact(n - k))

// Number of permutations on n objects using k at a time.
fn choose {n : nat}{ m : nat | m <= n } (n : int(n), k : int(m)) : Intinf =
  let
    fun numerator_loop { m : nat | m > 1 } .<m>. (i : int(m)) : [ n : nat | n > 0 ] intinf(n) =
      case+ i of
        | 1 => int2intinf(n)
        | 2 => $UN.cast(int2intinf(n - 1) * n)
        | i =>> $UN.cast((n + 1 - i) * numerator_loop(i - 1))
  in
    case+ k of
      | 0 => int2intinf(1)
      | 1 => int2intinf(n)
      | k =>> ndiv(numerator_loop(k), fact(k))
  end
