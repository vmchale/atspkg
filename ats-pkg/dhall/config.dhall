let version = "master"
in

{ defaultPkgs = "https://raw.githubusercontent.com/vmchale/atspkg/${version}/ats-pkg/pkgs/pkg-set.dhall"
, path = (None Text)
, githubUsername = ""
, filterErrors = False
}
