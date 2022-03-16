let
  pkgs = import <nixpkgs>{};
in
  pkgs.mkShell {
    name = "hcoreutils";
    buildInputs = with pkgs; with haskellPackages; [
      cabal-install
      haskell.compiler.ghc921
    ];
  }
