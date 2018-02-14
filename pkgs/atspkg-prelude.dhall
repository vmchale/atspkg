let showVersion = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/dhall-version.dhall
in
let makePkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/make-pkg.dhall
in

{ showVersion = showVersion, makePkg = makePkg }
