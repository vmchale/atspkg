let LibDep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/LibDep.dhall
in

let dep = λ(x : List Integer) →
  { dir = ".atspkg/contrib"
  , libVersion = x
    : List Integer
  , libDeps = []
    : List LibDep
  }
in dep [0,1,0]
