# Running using: $(nix-build --no-link -A fullBuildScript)
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
  nixpkgs               ? import <nixpkgs> {},
}:

let

  # Define basic configuration
  cabalPackageName = "kmonad";
  compiler         = "ghc865";

  # Grab the most recent version of `static-haskell-nix` (at time of writing)
  static-haskell-nix = nixpkgs.fetchFromGitHub {
    owner   = "nh2";
    repo    = "static-haskell-nix";
    rev     = "c6284b47f636c53230c98f6d115fd3f4561349f8";
    sha256  = "1mhkd4yfqcnr2hgap02kj1mcc4qvbmf317n2rfrx73xzd5rkx9y6";
  };

  # Import the pkgs provided by `static-haskell-nix`
  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  # Point the script at pwd, using a recent stackage snapshot
  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.;
    hackageSnapshot   = "2019-09-23T00:00:00Z";
  };

  # Setup the builder
  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

in
  {
    static_package = static-stack2nix-builder.static_package;
    inherit fullBuildScript;
    # For debugging:
    # inherit stack2nix-script;
    # inherit static-stack2nix-builder;
  }
