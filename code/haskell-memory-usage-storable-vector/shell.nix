{
  pinnedNixpkgsCommit ? "34f85de51bbc74595e63b22ee089adbb31f7c7a2", # nixos-20.09
  pinnedNixpkgsUrl ? "https://github.com/NixOS/nixpkgs/archive/${pinnedNixpkgsCommit}.tar.gz",
  pkgs ? import (fetchTarball pinnedNixpkgsUrl) {},
}:
let
in
  pkgs.haskell.lib.buildStackProject {
    ghc = pkgs.haskell.compiler.ghc865; # Keep in sync with the GHC version defined by stack.yaml!
    name = "myEnv";

    # System dependencies used only at build-time go in here.
    nativeBuildInputs = [
    ];

    # System dependencies used at build- and run-time go in here.
    buildInputs = [
    ];
  }
