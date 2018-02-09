let dep = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-pkg.dhall

in
  dep //
  { libName = "atscntrb-concurrency-0.1.0"
  , dir = ".atspkg/contrib"
  , url = "https://github.com/vmchale/ats-concurrency/archive/0.1.0.tar.gz"
  , libVersion = [0,1,0]
  -- , libDepends = []
  --   : List Text
  }
