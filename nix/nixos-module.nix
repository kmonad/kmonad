{ ... }:

{
  nixpkgs.overlays = [
    (final: prev: {
      kmonad = final.haskellPackages.callPackage ./default.nix { };
    })
  ];

  imports = [ ./module-base.nix ];
}
