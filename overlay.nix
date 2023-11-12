(self: super:
let
  gpipeSrc = super.fetchFromGitHub {
    # fork due to resizeBuffer PR https://github.com/tobbebex/GPipe-Core/pull/76
    # owner = "tobbebex";
    owner = "sorki";
    repo = "GPipe-Core";
    rev = "86a7b29014e7ebfb24ac17d5afcd877a38a1fbd5";
    sha256 = "08mvgygiq6i6vfjak4pq3cz1w3scvwv10igxn4vz6mna5fq6mnxz";
  };

  # until 2.0
  gpipeGlfwSrc = super.fetchFromGitHub {
     owner = "plredmond";
     repo = "GPipe-GLFW";
     rev = "3d7e91a20a80fe31e910884b151ebe4d26e8274e";
     sha256 = "0rx00mxlz6jlipx19h271mbnp8l9kzgm3dhwprwww4gf8g43r7vd";
  };
in
  ({
   haskellPackages = super.haskellPackages.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
           #GPipe = super.haskell.lib.doJailbreak hsuper.GPipe;
           GPipe = super.haskell.lib.doJailbreak (hsuper.callCabal2nix "GPipe" "${gpipeSrc}/GPipe-Core" {});
           GPipe-GLFW = super.haskell.lib.doJailbreak (hsuper.callCabal2nix "GPipe-GLFW" ("${gpipeGlfwSrc}/GPipe-GLFW") {});

           # for development
           # implicit = hsuper.callCabal2nix "implicit" (super.fetchFromGitHub {
           #    owner = "Haskell-Things";
           #    repo = "ImplicitCAD";
           #    rev = "ae794b901e9677593815fad741d87ff56846562d";
           #    sha256 = "0q8bj3jysgl7kfivrag8g6yx58n5rxf69qsc3lw43941lamaxpda";
           # }) {};
        });
      });
  })
)
