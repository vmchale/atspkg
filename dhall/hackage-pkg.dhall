let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/dhall-version.dhall

in λ(rec : { x : List Integer, name : Text }) → 
  dep //
    { libName = rec.name
    , dir = ".atspkg/contrib"
    , url = "https://hackage.haskell.org/package/${rec.name}-${showVersion rec.x}/${rec.name}-${showVersion rec.x}.tar.gz"
    , libVersion = rec.x
    }
