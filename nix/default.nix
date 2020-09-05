let
  # config = import ./config.nix;
  pkgs   = import (import ./pinned-nixpkgs.nix) {};
  kmonad = import ./kmonad.nix;
in with pkgs.haskellPackages; callPackage kmonad {}
