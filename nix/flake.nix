{
  description = "KMonad: An advanced keyboard manager.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      # List of supported systems:
      supportedSystems = [ "x86_64-linux" ];

      # List of supported compilers:
      supportedCompilers = [
        "ghc8107"
        "ghc902"
        "ghc922"
      ];

      # Function to generate a set based on supported systems:
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);

      # Attribute set of nixpkgs for each system:
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

      # A source file list cleaner for Haskell programs:
      haskellSourceFilter = src:
        nixpkgs.lib.cleanSourceWith {
          inherit src;
          filter = name: type:
            let baseName = baseNameOf (toString name); in
            nixpkgs.lib.cleanSourceFilter name type &&
            !(
              baseName == "dist-newstyle"
              || nixpkgs.lib.hasPrefix "." baseName
            );
        };

      # A fake version of git which can be used during the build to
      # embed the current revision into the binary despite the .git
      # directory not being available:
      fakeGit = pkgs: pkgs.writeShellScriptBin "git" ''
        echo ${self.rev or "dirty"}
      '';

      # The package derivation:
      derivation = pkgs: haskell: (
        haskell.callCabal2nix "kmonad" (haskellSourceFilter ../.) { }
      ).overrideAttrs (orig: {
        buildInputs = orig.buildInputs ++ [ (fakeGit pkgs) ];
      });
    in
    {
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in
        {
          # The full Haskell package for the default compiler:
          kmonad = derivation pkgs pkgs.haskellPackages;

          # Just the executables for the default compiler:
          default = pkgs.haskell.lib.justStaticExecutables
            (derivation pkgs pkgs.haskellPackages);
        } // builtins.listToAttrs (map
          (compiler: {
            name = "kmonad-${compiler}";
            value = derivation pkgs pkgs.haskell.packages.${compiler};
          })
          supportedCompilers));

      overlays.default = final: prev: {
        kmonad = self.packages.${prev.system}.default;
      };

      nixosModules.default = { ... }: {
        imports = [
          ./nixos-module.nix
          { nixpkgs.overlays = [ self.overlays.default ]; }
        ];
      };

      checks.x86_64-linux.default =
        let pkgs = nixpkgsFor.x86_64-linux; in
        import ./nixos-test.nix {
          inherit pkgs;
          module = self.nixosModules.default;
        };

      devShells = forAllSystems (system:
        let shellFor = haskell: name:
          haskell.shellFor {
            NIX_PATH = "nixpkgs=${nixpkgsFor.${system}.path}";

            packages = _: [ self.packages.${system}.${name} ];
            withHoogle = true;
            buildInputs = [
              haskell.cabal-fmt
              haskell.cabal-install
              haskell.ghcid
              haskell.haskell-language-server
              haskell.hlint
              haskell.ormolu
              haskell.stack
            ];
          }; in
        {
          default = shellFor
            nixpkgsFor.${system}.haskellPackages
            "kmonad";
        } // builtins.listToAttrs (map
          (compiler: {
            name = "kmonad-${compiler}";
            value = shellFor
              nixpkgsFor.${system}.haskell.packages.${compiler}
              "kmonad-${compiler}";
          })
          supportedCompilers));
    };
}
