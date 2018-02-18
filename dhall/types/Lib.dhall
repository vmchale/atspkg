let ForeignCabal = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/ForeignCabal.dhall
in
let TargetPair = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/TargetPair.dhall
in
let Lib = { name : Text, src : List Text, libTarget : Text, libs : List Text, includes : List Text, hsDeps : List ForeignCabal, hs2ats : List TargetPair, cSources : List Text, extras : List Text, static : Bool }
in

Lib
