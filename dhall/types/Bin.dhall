let ForeignCabal = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/ForeignCabal.dhall
in
let TargetPair = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/TargetPair.dhall
in
let Bin = { src : Text, target : Text, libs : List Text, hsDeps : List ForeignCabal , hs2ats : List TargetPair, gcBin : Bool, cSources : List Text, extras : List Text }
in

Bin
