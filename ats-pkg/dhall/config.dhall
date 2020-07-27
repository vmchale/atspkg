let commit = "96d6d6d5e29c4d8bafdc46725095e38a7c77d686"

let hash =
      "sha256:a16dc6b6d4d803a90682ec4e105a568a3c57bea8369fab6befccb9e6d203c615"

in  { defaultPkgs =
        "https://raw.githubusercontent.com/vmchale/atspkg/${commit}/ats-pkg/pkgs/pkg-set.dhall ${hash}"
    , path = None Text
    , githubUsername = ""
    , filterErrors = False
    }
