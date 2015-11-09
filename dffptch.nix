{ mkDerivation, aeson, base, bytestring, hspec, scientific, stdenv
, text, unordered-containers, vector
}:
mkDerivation {
  pname = "dffptch";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring hspec scientific text unordered-containers
    vector
  ];
  homepage = "https://github.com/paldepind/dffptch-haskell";
  description = "A small library for diffing and patching JSON objects with a compact diff format";
  license = stdenv.lib.licenses.mit;
}
