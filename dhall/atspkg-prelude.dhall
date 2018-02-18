let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/dhall-version.dhall
in
let makePkg = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/make-pkg.dhall
in
let bin = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-bin.dhall
in
let lib = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default-lib.dhall
in
let default = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/default.dhall
in
let plainDeps = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/plain-deps.dhall
in

{ showVersion = showVersion, makePkg = makePkg, bin = bin, lib = lib, default = default, plainDeps = plainDeps }
