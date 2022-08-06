{ mkDerivation, array, base, lib }:
mkDerivation {
  pname = "ToyPL";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base ];
  executableHaskellDepends = [ array base ];
  homepage = "https://github.com/githubuser/ToyPL#readme";
  license = lib.licenses.bsd3;
}
