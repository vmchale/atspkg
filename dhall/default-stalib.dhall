let lib = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-lib.dhall
in

lib
  // { static = True }
