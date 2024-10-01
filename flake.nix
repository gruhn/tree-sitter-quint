{
  description = "";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system: 
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # pkgs-staging = import (builtins.fetchGit {
        #   name = "Nixpkgs Staging";
        #   url = "https://github.com/NixOS/nixpkgs/";
        #   ref = "refs/heads/nixpkgs-unstable";
        #   rev = "d462c989c71e7686e7e89a0252e956cb04c08856";
        #  }) {};      
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.nodejs_22
            pkgs.typescript-language-server
            pkgs.tree-sitter
          ];
        };
      }
    );
}
