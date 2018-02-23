let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/dhall-version.dhall

in λ(version : List Integer) → 
  dep //
    { libName = "atscntrb-hx-intinf"
    , dir = ".atspkg/contrib/atscntrb-hx-intinf"
    , url = "https://registry.npmjs.org/atscntrb-hx-intinf/-/atscntrb-hx-intinf-${showVersion rec.x}.tgz"
    , libVersion = version
    }
