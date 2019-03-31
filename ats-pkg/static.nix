{ nixpkgs ? (import <nixpkgs> {}).pkgsMusl, compiler ? "ghc844", strip ? true }:


let

  pkgs = nixpkgs.pkgsMusl;

  ats-format = { mkDerivation, base, stdenv, bzlib
               , http-client, ansi-wl-pprint, cli-setup, lzma
               , composition-prelude, containers, dependency
               , dhall, file-embed, filemanip, http-client-tls,
               }:
      mkDerivation {
        pname = "ats-format";
        version = "0.2.0.39";
        src = pkgs.lib.sourceFilesBySuffices ./. [
          ".cabal"
          ".hs"
          ".1"
        ];
        isLibrary = true;
        isExecutable = true;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        libraryHaskellDepends = [ http-client bzlib ansi-wl-pprint
                                cli-setup lzma composition-prelude
                                containers dependency dhall
                                file-embed filemanip http-client-tls
                                ];
        license = stdenv.lib.licenses.bsd3;
        configureFlags = [
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ] ;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

in
  haskellPackages.callPackage ats-format {}
