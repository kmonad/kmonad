let
  config = import ./config.nix;
  pkgs   = import (import ./pinned-nixpkgs.nix) { inherit config; };
in with pkgs.haskellPackages; callPackage kmonad { doHaddock=false;}
