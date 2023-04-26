{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;
        hsDevPkgs = hsPkgs.ghcWithPackages(p: with p;
          [ # packages used in effective haskell
            bytestring
            base64-bytestring
            text
            containers
            vector
            time
            unix
            mtl
            transformers
            process

            # testing packages
            hspec
            hspec-expectations

            # additional packages used in bonus content
            hashable
          ]);
        builder = returnShellEnv:
          import ./builder/default.nix { inherit pkgs hsPkgs returnShellEnv;};
        site = pkgs.stdenv.mkDerivation {
          name = "effective-haskell.com";
          doCheck = true;
          src = pkgs.nix-gitignore.gitignoreSourcePure [
            ".git"
          ] ./site;
          buildInputs = [self.packages.${system}.builder pkgs.linkchecker];
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LC_ALL = "C.UTF-8";
          buildPhase = ''
            ${self.packages.${system}.builder}/bin/builder build
          '';
          checkPhase = ''
            linkchecker _site
          '';
          installPhase = ''
            cp -r _site $out
            cp -r CNAME $out
          '';
        };

        shellPackages =
          (with hsPkgs; [fourmolu ghcid hlint]) ++
          (with pkgs; [ linkchecker ]) ++
          [ hsDevPkgs ];

         buildAndWatchScript = pkgs.writeScriptBin "buildAndWatch" ''
          cd site;
          ${self.packages.${system}.builder}/bin/builder clean;
          ${self.packages.${system}.builder}/bin/builder watch;
'';
        buildAndWatch = {
          type = "app";
          program = "${self.packages.${system}.buildAndWatchScript}/bin/buildAndWatch";
        };

      in {
        packages.builder = builder false;
        packages.site = site;
        packages.buildAndWatchScript = buildAndWatchScript;
        apps.buildAndWatch = buildAndWatch;
        defaultPackage = self.packages.${system}.builder;
        defaultApp = self.apps.${system}.buildAndWatch;
        devShell = pkgs.mkShell {
          inputsFrom = [(builder true)] ++ shellPackages;
          buildInputs = shellPackages;
        };
      });
}
