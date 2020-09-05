{ mkDerivation, base, cereal, lens, megaparsec, mtl
, optparse-applicative, resourcet, rio, stdenv, time, unix
, unliftio
}:
mkDerivation {
  pname = "kmonad";
  version = "0.4";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cereal lens megaparsec mtl optparse-applicative resourcet rio
    time unix unliftio
  ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  description = "Advanced keyboard remapping utility";
  license = stdenv.lib.licenses.mit;
}
