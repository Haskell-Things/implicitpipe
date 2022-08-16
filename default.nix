{ rev ? "7c6985653708c5fade76d2014824ff333b0a07b2"
, overlays ? [ (import ./overlay.nix) ]
, pkgs ?
    if ((rev == "") || (rev == "default") || (rev == "local"))
      then import <nixpkgs> { inherit overlays; }
      # Do not guard with hash, so the project is able to use current channels (rolling `rev`) of Nixpkgs
      else import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") { inherit overlays; }
}:

let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  myGhc = pkgs.haskellPackages.ghcWithPackages (ps: with pkgs.haskellPackages; [
    implicit
    linear
  ]);

  pkg =
    #pkgs.haskell.lib.buildFromSdist (
    #pkgs.haskell.lib.justStaticExecutables (
      pkgs.haskellPackages.callCabal2nix "implicitpipe" src {}
    #)
    #)
    ;

in
{
  passthru = { inherit pkg; };
  # based on https://github.com/haskell-hint/hint/issues/79
  wrapper = pkgs.stdenv.mkDerivation {
      name = "implicitpipe-wrapped";
      unpackPhase = "true";
      buildInputs = [ myGhc pkg pkgs.makeWrapper ];
      buildPhase = ''
        # We need to provide the Haskell interpreter (hint) with the location of the ghc lib dir and the package db
        mkdir -p $out/bin
        ln -s ${pkg}/bin/implicitanim $out/bin/implicitanim
        ln -s ${pkg}/bin/implicitview $out/bin/implicitview
        wrapProgram $out/bin/implicitview \
          --set GHC_PACKAGE_PATH "${myGhc}/lib/${myGhc.meta.name}/package.conf.d"
        '';
      installPhase = "echo nothing to install";
  };
}
