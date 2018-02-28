let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in prelude.dep //
  { libName = "atscntrb-hx-divideconquer"
  , dir = ".atspkg/contrib/atscntrb-bucs320-divideconquer"
  , url = "https://registry.npmjs.org/atscntrb-bucs320-divideconquer/-/atscntrb-bucs320-divideconquer-1.0.5.tgz"
  , libVersion = [1,0,5]
  , libDeps = prelude.mapPlainDeps [ "atscntrb-hx-fworkshop", "atscntrb-hx-threadkit" ]
  }
