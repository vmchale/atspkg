{ mkDerivation, ansi-wl-pprint, base, binary, bytestring, bzlib
, Cabal, cli-setup, composition-prelude, containers, cpphs
, dependency, dhall, directory, file-embed, filemanip, filepath
, http-client, http-client-tls, lzma, microlens, mtl
, optparse-applicative, parallel-io, process, shake, shake-ats
, shake-c, shake-ext, stdenv, tar, temporary, text, unix
, zip-archive, zlib
}:
mkDerivation {
  pname = "ats-pkg";
  version = "3.2.1.8";
  sha256 = "183gdyivl6kab2k3z0jm6dk0wh83qwz3zvai7ayfkq3rjc6lb8ms";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base binary bytestring bzlib Cabal
    composition-prelude containers dependency dhall directory
    file-embed filemanip filepath http-client http-client-tls lzma
    microlens mtl parallel-io process shake shake-ats shake-c shake-ext
    tar text unix zip-archive zlib
  ];
  libraryToolDepends = [ cpphs ];
  executableHaskellDepends = [
    base bytestring cli-setup dependency directory microlens
    optparse-applicative parallel-io shake shake-ats temporary text
  ];
  doHaddock = false;
  description = "A build tool for ATS";
  license = stdenv.lib.licenses.bsd3;
}
