{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "fp-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/fp-parser#readme";
  license = stdenv.lib.licenses.bsd3;
}
