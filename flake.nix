{
  description = "Nanopass compiler framework in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: 
      let
        pkgs = import nixpkgs { inherit system; };

        forGhc = {version}: pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.${version}
            cabal-install
            haskell.packages.${version}.haskell-language-server
          ];
        };
      in {
        devShells.default = forGhc { version = "ghc912"; };

        devShells.ghc912 = forGhc { version = "ghc912"; };
      }
    );
}
