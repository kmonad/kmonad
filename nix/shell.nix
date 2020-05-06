let
  nixpkgs = import ./release.nix;
in
  nixpkgs.haskellPackages.shellFor {
    packages = p: [p.kmonad];
    withHoogle = true;
    buildInputs = [
      nixpkgs.haskellPackages.cabal-install
    ];
  }
