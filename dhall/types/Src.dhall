let TargetPair = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/TargetPair.dhall
in
let Src = { src : Text, cTarget : Text, libs : List Text, atsGen : List TargetPair, extras : List Text }
in

Bin
