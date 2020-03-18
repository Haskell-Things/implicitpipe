(self: super:
  ({
   haskellPackages = super.haskellPackages.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
           GPipe = super.haskell.lib.doJailbreak hsuper.GPipe;
           GPipe-GLFW = super.haskell.lib.doJailbreak hsuper.GPipe-GLFW;
        });
      });
  })
)
