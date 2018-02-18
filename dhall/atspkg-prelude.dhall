let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/dhall-version.dhall
in
let makePkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/make-pkg.dhall
in
let bin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall
in
let lib = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-lib.dhall
in
let default = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall
in

{ showVersion = showVersion, makePkg = makePkg, bin = bin, lib = lib, default = default }
