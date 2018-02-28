let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in λ(v : List Integer) → 
  prelude.dep //
    { libName = "atscntrb-hx-intinf"
    , dir = ".atspkg/contrib/atscntrb-hx-intinf"
    , url = "https://registry.npmjs.org/atscntrb-hx-intinf/-/atscntrb-hx-intinf-${prelude.showVersion v}.tgz"
    , libVersion = v
    , libDeps = prelude.mapPlainDeps [ "atscntrb-libgmp" ]
    }
