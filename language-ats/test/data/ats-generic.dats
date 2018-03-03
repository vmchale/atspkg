#include "share/atspre_staload.hats"

#define ATS_MAINATSFLAG 1

staload UN = "prelude/SATS/unsafe.sats"

vtypedef boring_type = @{ first = int, second = int }

val return_boring_type: boring_type = @{ first = 12, second = 13 }

extern
val pass_boring_type : boring_type =
  "mac#"

implement pass_boring_type =
  return_boring_type
