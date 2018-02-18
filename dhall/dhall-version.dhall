let concatMapSep = https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/Text/concatMapSep
in

let showVersion = λ(x : List Integer) → concatMapSep "." Integer (Integer/show) x

in showVersion
