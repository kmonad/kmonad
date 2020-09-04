# Insert our own packages into the ghc883 package-set
{
  allowUnfree = true;
  packageOverrides = p: rec {
    haskellPackages = p.haskell.packages.ghc883.override {
      overrides = self: super:
        {
          kmonad = super.callCabal2nixWithOptions "kmonad" ../. "--no-haddock" {};
        };
    };
  };
}
