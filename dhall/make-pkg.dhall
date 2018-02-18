let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-pkg.dhall
in
let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/dhall-version.dhall

in λ(rec : { x : List Integer, name : Text, githubUsername : Text}) → 
  dep //
    { libName = rec.name
    , dir = ".atspkg/contrib"
    , url = "https://github.com/${rec.githubUsername}/${rec.name}/archive/${showVersion rec.x}.tar.gz"
    , libVersion = rec.x
    }
