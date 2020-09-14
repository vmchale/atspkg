let commit = "137c57c95135591c6627fffcc1ba75864b4c0918"

let hash =
      "sha256:fa901519fa579dbb5ee187bc2c43c20b3c1f8ad2ff9129f393f1066a174b413d"

in  { defaultPkgs =
        "https://raw.githubusercontent.com/vmchale/atspkg/${commit}/ats-pkg/pkgs/pkg-set.dhall ${hash}"
    , path = None Text
    , githubUsername = ""
    , filterErrors = False
    }
