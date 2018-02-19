let makePkg = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/make-pkg.dhall

in λ(x : { x : List Integer, name : Text, githubUsername : Text, description : Text }) →
  makePkg { x = x.x, name = x.name, githubUsername = x.githubUsername }
    // { description = [ x.description ] : Optional Text }
