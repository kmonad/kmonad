{ sources ? import ./nix/sources.nix, pkgs ? (import sources.nixpkgs { }) }:

let
  def = import ./nix/default.nix { };

  dev-pkgs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
    haskell-language-server
    hlint
    ormolu
    stack
  ];

in def.pkg.overrideAttrs (attrs: {
  src = null;
  buildInputs = dev-pkgs ++ attrs.buildInputs;
})
