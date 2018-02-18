let LibDep = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/types/LibDep.dhall
in
let plainDeps = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/plain-deps.dhall
in
let map = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/List/map
in

let mapPlainDeps = λ(x : List Text) → map Text LibDep plainDeps x
in mapPlainDeps
