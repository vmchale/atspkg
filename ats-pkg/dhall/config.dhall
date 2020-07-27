let commit = "96d6d6d5e29c4d8bafdc46725095e38a7c77d686"

let hash =
      "sha256:c04fe26a86f2e2bd5c67c17f213ee30379d520f5fad11254a8f17e936250e27e"

in  { defaultPkgs =
        "https://raw.githubusercontent.com/vmchale/atspkg/${commit}/ats-pkg/pkgs/pkg-set.dhall ${hash}"
    , path = None Text
    , githubUsername = ""
    , filterErrors = False
    }
