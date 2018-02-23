let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in λ(x : List Integer) → 
  prelude.makeHsPkg { x = x, name = "fast-arithmetic" } 
    // { libDeps = prelude.mapPlainDeps [ "atscntrb-hx-intinf" ], description = [ "Number theory & combinatorics library written in ATS" ] : Optional Text }
