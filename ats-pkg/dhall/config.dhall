let commit = "a534139794e1449e9c74a00487689f4ea6d7a3a9"

let hash =
      "sha256:bb0ac19def18b7d752c4675534c6c5af779aa1b8fea13ace8aeee1231e950e91"

in  { defaultPkgs =
        "https://raw.githubusercontent.com/vmchale/atspkg/${commit}/ats-pkg/pkgs/pkg-set.dhall ${hash}"
    , path = None Text
    , githubUsername = ""
    , filterErrors = False
    }
