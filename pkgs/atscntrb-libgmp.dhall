let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/dhall-version.dhall

in λ(v : List Integer) → 
  dep //
    { libName = "atscntrb-libgmp"
    , dir = ".atspkg/contrib/atscntrb-libgmp"
    , url = "https://registry.npmjs.org/atscntrb-libgmp/-/atscntrb-libgmp-${showVersion v}.tgz"
    , url = "https://registry.npmjs.org/atscntrb-hx-intinf/-/atscntrb-hx-intinf-${showVersion v}.tgz"
    , libVersion = v
    }
