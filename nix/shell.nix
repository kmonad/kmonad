let
  config = import ./config.nix;
  pkgs   = import (import ./pinned-nixpkgs.nix) { inherit config; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [p.kmonad];
    withHoogle = true;
    buildInputs = [
      pkgs.haskellPackages.cabal-install
    ];
  }
