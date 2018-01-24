(* ****** ****** *)
(*
** For writing ATS code
** that translates into PHP
*)
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)
//
// HX-2014-08:
// prefix for external names
//
#define
ATS_EXTERN_PREFIX "ats2phppre_"
#define
ATS_STATIC_PREFIX "_ats2phppre_string_"
//
(* ****** ****** *)
//
#staload
UN =
"prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#staload "./../basics_php.sats"
//
(* ****** ****** *)
//
#staload "./../SATS/integer.sats"
//
(* ****** ****** *)
//
#staload "./../SATS/string.sats"
#staload "./../SATS/PHParray.sats"
//
(* ****** ****** *)
//
#staload "./../SATS/stream.sats"
#staload "./../SATS/stream_vt.sats"
//
#staload _(*STREAM*) = "./stream.dats"
#staload _(*STREAM_VT*) = "./stream_vt.dats"
//
(* ****** ****** *)
//
implement
string_explode(cs) =
(
  PHParray2list(PHParray_of_string(cs))
)
//
(* ****** ****** *)

(* end of [string.dats] *)
