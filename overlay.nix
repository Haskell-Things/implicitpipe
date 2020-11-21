(self: super:
  ({
   haskellPackages = super.haskellPackages.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
           GPipe = super.haskell.lib.doJailbreak hsuper.GPipe;
           GPipe-GLFW = super.haskell.lib.doJailbreak hsuper.GPipe-GLFW;
           # until > 3.0.2 is out
           implicit = hsuper.callCabal2nix "implicit" (super.fetchFromGitHub {
              owner = "colah";
              repo = "ImplicitCAD";
              rev = "5ddd75006cc8f1d6a0e184f56c3714a9097ce6b0";
              sha256 = "0mmhw9xwhlp5aa3ls44m7v7lv6l8h548w49hz8rn55r0nfpzy5z7";
           }) {};
        });
      });
  })
)
