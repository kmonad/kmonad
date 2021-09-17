{ sources ? import ./sources.nix, pkgs ? (import sources.nixpkgs { }) }:

let
  t = pkgs.lib.trivial;
  hl = pkgs.haskell.lib;

  deps = [
    pkgs.git # Necessary to compile KMonad, but not to run it.
  ];

  pkg = pkgs.haskellPackages.developPackage {

    root = ./..;

    modifier = (t.flip t.pipe) [
      (drv: hl.addBuildDepends drv deps) # Insert our buildDepends
      hl.justStaticExecutables # Only build the executable

      # TODO: investigate these here: https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix
      # hl.dontHaddock
      # hl.enableStaticLibraries
      # hl.disableLibraryProfiling
      # hl.disableExecutableProfiling
      #
      # Maybe I make multiple targets, 1 for the executable, 1 for the hackage docs?
    ];
  };

in { inherit pkg; }
