{ mkDerivation, base, cereal, lens, lib, megaparsec, mtl
, optparse-applicative, resourcet, rio, time, unix
, unliftio, pkgs
}:
mkDerivation {
  pname = "kmonad";
  version = "0.4.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cereal lens megaparsec mtl optparse-applicative resourcet rio
    time unix unliftio
  ];
  buildDepends = [ pkgs.git ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  description = "Advanced keyboard remapping utility";
  license = lib.licenses.mit;
}
