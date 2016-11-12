# GPipe-Test

This is a simple example of how to use the new GPipe API with the GPipe-GLFW backend.
This example has been lifted almost verbatim from
[GPipe is dead, long live GPipe!](http://tobbebex.blogspot.com/2015/09/gpipe-is-dead-long-live-gpipe.html).
All credit and ownership goes to Tobias Bexelius.

## Building

* Cabal

  * With Nix: [Add this project to `~/.nixpkgs/config.nix`](https://nixos.org/nixpkgs/manual/#how-to-build-projects-that-depend-on-each-other) and then use either `nix-shell` or `nix-build`.
    * `nix-shell "<nixpkgs>" -A haskellPackages.GPipe-Test.env` and then `cabal build`
    * `nix-build "<nixpkgs>" -A haskellPackages.GPipe-Test`
  * Without Nix: Install the system package dependencies listed in `stack.yaml` using your system's package manager. It is recommended that you use sandboxes to allow Cabal's resolver to pick Haskell dependencies.

* Stack

  * With Nix: Use `stack build` and the [system package dependencies listed in `stack.yaml` will be fetched for a build shell](https://github.com/commercialhaskell/stack/blob/master/doc/nix_integration.md).
  * Without Nix: Install the system package dependencies listed in `stack.yaml` using your system's package manager. Then build with `stack build`.

## Changes

* 0.1.0.2
    * Use GPipe-GLFW 1.2.3
    * Correct some ghc warnings, ignore others
* 0.1.0.1
    * Changed how input state is output & added a scroll callback example.
    * [pakanek](https://github.com/pakanek) added a missing min-version constraint.

# What is GPipe?

**GPipe** is a typesafe functional API based on the conceptual model of OpenGL,
but without the imperative state machine.

To learn GPipe, start with the [readme](https://github.com/tobbebex/GPipe-Core#readme).
You could also go directly to [a working example](https://github.com/plredmond/GPipe-Test)
or the tutorials
[Part 1](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe.html),
[Part 2](http://tobbebex.blogspot.se/2015/09/gpu-programming-in-haskell-using-gpipe_11.html),
[Part 3](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe.html),
[Part 4](http://tobbebex.blogspot.se/2015/10/gpu-programming-in-haskell-using-gpipe_21.html),
[Part 5](http://tobbebex.blogspot.se/2015/11/gpu-programming-in-haskell-using-gpipe.html).

Find GPipe on
[github](https://github.com/tobbebex/GPipe-Core),
[stackage](https://www.stackage.org/package/GPipe), and
[hackage](https://hackage.haskell.org/package/GPipe).