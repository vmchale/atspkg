let version = "master"
in

let cfg =
  { defaultPkgs = "https://raw.githubusercontent.com/vmchale/atspkg/${version}/ats-pkg/pkgs/pkg-set.dhall"
  , path = ([] : Optional Text)
  , githubUsername = ""
  , filterErrors = False
  }
in cfg
