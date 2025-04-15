{
  description = "My Haskell project";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        
        packageName = "student-correction-transformer"; # Replace with your package name
        
        package = haskellPackages.callCabal2nix packageName ./. {};
        
      in {
        packages.${packageName} = package;
        defaultPackage = package;
        
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskell.compiler.ghc94 # Choose appropriate GHC version
          ];
        };
      }
    );
}