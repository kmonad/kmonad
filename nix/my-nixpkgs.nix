let
  config  = import ./config.nix;
  nixpkgs = import (import ./pinned-nixpkgs.nix) { inherit config; };
in nixpkgs
