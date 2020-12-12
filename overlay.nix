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
     rev = "83d26eb7b41d67f5ac6fbd1bd8758d72c660e039";
     sha256 = "0fg60amvp2v37cwmvfa0n7if1ppisjjh3bknmrr17m7fbfbbxlhq";
  };
in
  ({
   haskellPackages = super.haskellPackages.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
           #GPipe = super.haskell.lib.doJailbreak hsuper.GPipe;
           GPipe = hsuper.callCabal2nix "GPipe" "${gpipeSrc}/GPipe-Core" {};
           GPipe-GLFW = hsuper.callCabal2nix "GPipe-GLFW" ("${gpipeGlfwSrc}/GPipe-GLFW") {};

           # until > 3.0.2 is out
           implicit = hsuper.callCabal2nix "implicit" (super.fetchFromGitHub {
              owner = "colah";
              repo = "ImplicitCAD";
              rev = "8dff5531cdc4d9ed32bf958e3945b4a3a0ef3774";
              sha256 = "0bp797a9wlpyw2d6b4csz5ikqq3wy1qry0iabl7r7axjrhvnfp56";
           }) {};
        });
      });
  })
)
