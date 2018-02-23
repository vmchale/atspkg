let ForeignCabal = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/ForeignCabal.dhall
in
let TargetPair = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/TargetPair.dhall
in

let lib =
  { libs = ([] : List Text)
  , includes = ([] : List Text)
  , hsDeps = ([] : List ForeignCabal)
  , hs2ats = ([] : List TargetPair)
  , links = ([] : List { _1 : Text, _2 : Text })
  , cSources = ([] : List Text)
  , extras = ([] : List Text)
  , static = True
  }
in lib
