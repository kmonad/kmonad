{ ... }:

{
  nixpkgs.overlays = [ (final: prev: { kmonad = import ./default.nix; }) ];
  imports = [ ./module-base.nix ];
}
