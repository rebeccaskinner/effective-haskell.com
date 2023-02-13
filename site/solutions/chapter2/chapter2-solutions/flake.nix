{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        mkProject = returnShellEnv: import ./project.nix { inherit pkgs returnShellEnv; };
        runTests = pkgs.writeScriptBin "runTests" ''
        make test
'';
        runTestApps = {
          type = "app";
          program = "${self.packages.${system}.runTests}/bin/runTests";
          buildInputs = [runTests (mkProject false)];
        };
      in {
        packages.runTests = runTests;
        devShells.default = pkgs.mkShell { inputsFrom = [(mkProject true)]; };
        apps.test = runTestApps;
      });
}
