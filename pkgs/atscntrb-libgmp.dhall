let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default.dhall
in

in λ(v : List Integer) → 
  prelude.dep //
    { libName = "atscntrb-libgmp"
    , dir = ".atspkg/contrib/atscntrb-libgmp"
    , url = "https://registry.npmjs.org/atscntrb-libgmp/-/atscntrb-libgmp-${prelude.showVersion v}.tgz"
    , libVersion = v
    }
