(import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }).haskellPackages.callCabal2nix "GPipe-Test" ./. {}
