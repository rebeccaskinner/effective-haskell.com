{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        shellEnv = import ./project.nix { inherit pkgs;  returnShellEnv = true; };
        lib = import ./project.nix { inherit pkgs; returnShellEnv = false; };
      in {

        packages.default = lib;
        devShells.default = pkgs.mkShell {
          inputsFrom = [shellEnv];
        };
      });
  nixConfig.bash-prompt = "\\u@\\h:\\W (nix) λ ";
}
