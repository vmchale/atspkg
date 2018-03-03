#include ".atspkg/contrib/hs-bind-0.3.6/runtime.dats"
#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"
staload "src/gen.sats"

fun free_option(x : option(int)) : void =
  case+ x of
    | ~Some (_) => ()
    | ~None() => ()

fun print_pair(x : pair(int, int)) : void =
  let
    val f = x.first
    val s = x.second
  in
    println!("Pair { first = ", f, ", second = ", s, " }")
  end

fun print_option_int(x : !option(int)) : void =
  case+ x of
    | Some (n) => (print("Some") ; println!(n))
    | None() => println!("None")

extern
fun hs_pass() : ptr =
  "mac#pass_val"

extern
fun hs_read() : ptr =
  "mac#read_dhall"

implement main0 (argc, argv) =
  {
    val _ = hs_init(argc, argv)
    var x = $UN.ptr0_get<option(int)>(hs_read())
    val y = $UN.ptr0_get<pair(int, int)>(hs_pass())
    val _ = print_pair(y)
    val _ = print_option_int(x)
    val _ = free_option(x)
    val _ = hs_exit()
  }
