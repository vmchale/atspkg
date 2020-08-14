let commit = "71dc4637adcb11863a0010b6bcde64cf5c0e38d7"

let hash =
      "sha256:d680b2c6b8228d6179c6121d2b2e936e7e99d59f00f8f3bdc8f2fdbf0acafbc7"

in  { defaultPkgs =
        "https://raw.githubusercontent.com/vmchale/atspkg/${commit}/ats-pkg/pkgs/pkg-set.dhall ${hash}"
    , path = None Text
    , githubUsername = ""
    , filterErrors = False
    }
