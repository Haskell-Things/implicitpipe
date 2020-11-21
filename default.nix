(import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }).haskellPackages.callCabal2nix "implicitpipe" ./. {}
