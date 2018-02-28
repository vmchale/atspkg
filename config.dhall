let version = "master"
in

let cfg =
  { defaultPkgs = "https://raw.githubusercontent.com/vmchale/atspkg/${version}/pkgs/pkg-set.dhall"
  , path = ([] : Optional Text)
  , githubUsername = ""
  , filterErrors = False
  }
in cfg
