{ mkDerivation, base, GPipe, GPipe-GLFW, JuicyPixels, linear
, stdenv
}:
mkDerivation {
  pname = "GPipe-Test";
  version = "0.1.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base GPipe GPipe-GLFW JuicyPixels linear
  ];
  license = stdenv.lib.licenses.mit;
}
