{ pkgs ? import <nixpkgs> {}
, returnShellEnv ? false
}:
let
  hsPkgs = pkgs.haskellPackages;
in
pkgs.haskellPackages.developPackage {
  name = "chapter1-solutions";
  root = pkgs.nix-gitignore.gitignoreSourcePure
    [ "dist-newstyle"
      ".*#"
      ".git"
    ] ./.;
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs:
    let
      prevTools = attrs.buildTools or [];
      requiredTools = [hsPkgs.fourmolu hsPkgs.ghcid];
    in {buildTools = prevTools ++ requiredTools;});
  inherit returnShellEnv;
}
