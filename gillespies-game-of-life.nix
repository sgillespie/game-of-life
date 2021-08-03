{ mkDerivation, base, lib }:
mkDerivation {
  pname = "gillespies-game-of-life";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/sgillespie/game-of-life";
  description = "Submission for GDFG's Monthly Game Jam #8";
  license = lib.licenses.bsd3;
}
