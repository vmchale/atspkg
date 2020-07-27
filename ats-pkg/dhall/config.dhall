let commit = "6c11f86a1c847379eb7414f2a1a42b7318e588d6"

let hash =
      "sha256:38298d16f3b5e34d93e4401ce5395c00af3eb24644b012deceb19ef8a0973311"

in  { defaultPkgs =
        "https://raw.githubusercontent.com/vmchale/atspkg/${commit}/ats-pkg/pkgs/pkg-set.dhall ${hash}"
    , path = None Text
    , githubUsername = ""
    , filterErrors = False
    }
