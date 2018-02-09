let pkgset =
  [ https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquer.dhall
  , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/atscntrb-hx-divideconquerpar.dhall
  , https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall 
  ] : List { dir : Text, libName : Text, libVersion : List Integer, url : Text }

in pkgset
