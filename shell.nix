let
  config = import ./nix/config.nix;
  pkgs   = import (import ./nix/pinned-nixpkgs.nix) { inherit config; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [p.kmonad];
    withHoogle = true;
    buildInputs = [
      pkgs.haskellPackages.cabal-install
    ];
  }
