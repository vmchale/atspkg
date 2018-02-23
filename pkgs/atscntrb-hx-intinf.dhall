let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/dhall-version.dhall

in λ(v : List Integer) → 
  dep //
    { libName = "atscntrb-hx-intinf"
    , dir = ".atspkg/contrib/atscntrb-hx-intinf"
    , url = "https://registry.npmjs.org/atscntrb-hx-intinf/-/atscntrb-hx-intinf-${showVersion v}.tgz"
    , libVersion = v
    libDeps = prelude.mapPlainDeps [ "atscntrb-libgmp" ]
    }
