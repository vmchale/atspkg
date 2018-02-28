let ForeignCabal = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/ForeignCabal.dhall
in
let TargetPair = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/TargetPair.dhall
in

let bin =
  { libs = ([] : List Text)
  , hsDeps = ([] : List ForeignCabal)
  , hs2ats = ([] : List TargetPair)
  , gcBin = False
  , extras = ([] : List Text)
  }
in bin
