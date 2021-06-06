(self: super:
let
  gpipeSrc = super.fetchFromGitHub {
    owner = "homectl";
    repo = "GPipe-Core";
    rev = "273f58fc53e8560ed72f0e62867b96e4afec92c3";
    sha256 = "06lm8mj7d5lpi5f8cgas4rx1xq5wagb9n3j7cfip2zckwrq7rl5j";
  };

  gpipeGlfwSrc = super.fetchFromGitHub {
     owner = "homectl";
     repo = "GPipe-GLFW4";
     rev = "999b55e2cf78c052884f5ec9ab154e3cc399ba7a";
     sha256 = "09182qs5cf5glhxavcp24f74f1kkk5pfdwmah2rg31ggz1wa5m81";
  };
in
  ({
   haskellPackages = super.haskellPackages.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
           #GPipe = super.haskell.lib.doJailbreak hsuper.GPipe;
           GPipe = hsuper.callCabal2nix "GPipe" "${gpipeSrc}/GPipe-Core" {};
           GPipe-GLFW = hsuper.callCabal2nix "GPipe-GLFW" ("${gpipeGlfwSrc}/GPipe-GLFW") {};

           # until > 3.0 is out
           implicit = hsuper.callCabal2nix "implicit" (super.fetchFromGitHub {
              owner = "colah";
              repo = "ImplicitCAD";
              rev = "67ab4ccc046e255e36c49e40ae2ceedda6a49400";
              sha256 = "1w4xjdxgc9mfjm681pdnhzcxvppvmn9p381gpfl30b8rvbgzjj4d";
           }) {};
        });
      });
  })
)
