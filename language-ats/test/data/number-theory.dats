#include "share/atspre_staload.hats"
#include "ats-src/numerics.dats"
#include "contrib/atscntrb-hx-intinf/mylibies.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload "contrib/atscntrb-hx-intinf/SATS/intinf_vt.sats"

#define ATS_MAINATSFLAG 1

// Existential types for even and odd numbers. These are only usable with the
// ATS library.
typedef Even = [ n : nat ] int(2 * n)

typedef Odd = [ n : nat ] int(2 * n+1)

// TODO jacobi symbol
// fn legendre(a: int, p: int) : int =
//  a ^ (p - 1 / 2) % p
// m | n
fn divides(m : int, n : int) :<> bool =
  n % m = 0

fnx gcd {k : nat}{l : nat} (m : int(l), n : int(k)) : int =
  if n > 0 then
    gcd(n, witness(m % n))
  else
    m

fn lcm {k : nat}{l : nat} (m : int(l), n : int(k)) : int =
  (m / gcd(m, n)) * n

// stream all divisors of an integer.
fn divisors(n : intGte(1)) :<> stream_vt(int) =
  let
    fun loop {k : nat}{ m : nat | m > 0 && k >= m } .<k-m>. (n : int(k), acc : int(m)) :<> stream_vt(int) =
      if acc >= n then
        $ldelay(stream_vt_cons(acc, $ldelay(stream_vt_nil)))
      else
        if n % acc = 0 then
          $ldelay(stream_vt_cons(n, loop(n, acc + 1)))
        else
          $ldelay(stream_vt_nil)
  in
    loop(n, 1)
  end

// stream all prime divisors of an integer (without multiplicity)
fn prime_divisors(n : intGte(1)) :<> stream_vt(int) =
  let
    fun loop {k : nat}{ m : nat | m > 0 && k >= m } .<k-m>. (n : int(k), acc : int(m)) :<> stream_vt(int) =
      if acc >= n then
        $ldelay(stream_vt_cons(acc, $ldelay(stream_vt_nil)))
      else
        if n % acc = 0 && is_prime(n) then
          $ldelay(stream_vt_cons(n, loop(n, acc + 1)))
        else
          $ldelay(stream_vt_nil)
  in
    loop(n, 1)
  end

fn witness(n : int) : intGte(0) =
  $UN.cast(n)

propdef PRIME (p : int) = { x, y : nat | x <= y } MUL(x, y, p) -<> [ x == 1 ] void

dataprop FACT(int, int) =
  | FACTbas(0, 1)
  | {n : nat}{ r, r1 : int } FACTind(n + 1, r) of (FACT(n, r1), MUL(n + 1, r1, r))

fun get_multiplicity { p : nat | p > 1 } (n : intGte(0), p : int(p)) : int =
  case+ n % p of
    | 0 => 1 + get_multiplicity(witness(n / p), p)
    | _ => 0

fn count_divisors(n : intGte(1)) :<> int =
  let
    fun loop {k : nat}{ m : nat | m > 0 && k >= m } .<k-m>. (n : int(k), acc : int(m)) :<> int =
      if acc >= n then
        1
      else
        if n % acc = 0 then
          1 + loop(n, acc + 1)
        else
          loop(n, acc + 1)
  in
    loop(n, 1)
  end

fn sum_divisors(n : intGte(1)) :<> int =
  let
    fun loop {k : nat}{ m : nat | m > 0 && k >= m } .<k-m>. (n : int(k), acc : int(m)) :<> int =
      if acc >= n then
        0
      else
        if n % acc = 0 then
          acc + loop(n, acc + 1)
        else
          loop(n, acc + 1)
  in
    loop(n, 1)
  end

fn is_perfect(n : intGte(1)) :<> bool =
  sum_divisors(n) = n

// distinct prime divisors
fn little_omega(n : intGte(1)) :<> int =
  let
    fun loop {k : nat}{ m : nat | m > 0 && k >= m } .<k-m>. (n : int(k), acc : int(m)) :<> int =
      if acc >= n then
        if is_prime(n) then
          1
        else
          0
      else
        if n % acc = 0 && is_prime(acc) then
          1 + loop(n, acc + 1)
        else
          loop(n, acc + 1)
  in
    loop(n, 1)
  end

// Euler's totient function.
fn totient(n : intGte(1)) : int =
  case+ n of
    | 1 => 1
    | n =>> 
      begin
        let
          fnx loop { k : nat | k >= 2 }{ m : nat | m > 0 && k >= m } .<k-m>. (i : int(m), n : int(k)) : int =
            if i >= n then
              if is_prime(n) then
                n - 1
              else
                n
            else
              if n % i = 0 && is_prime(i) && i != n then
                (loop(i + 1, n) / i) * (i - 1)
              else
                loop(i + 1, n)
        in
          loop(1, n)
        end
      end

// TODO modular exponentiation
// The sum of all φ(m) for m between 1 and n 
fun totient_sum(n : intGte(1)) : Intinf =
  let
    fnx loop { n : nat | n >= 1 }{ m : nat | m >= n } .<m-n>. (i : int(n), bound : int(m)) : Intinf =
      if i < bound then
        let
          val x = loop(i + 1, bound)
          val y = add_intinf0_int(x, witness(totient(i)))
        in
          y
        end
      else
        int2intinf(witness(totient(i)))
  in
    loop(1, n)
  end

extern
fun chinese_remainder {n : nat} (residues : list_vt(int, n), moduli : list_vt(int, n)) : Option_vt(int)