{ pkgs ? import <nixpkgs> {}
, hsPkgs ? pkgs.haskellPackages
, addPkgs ? (pkgs: hsPkgs: [])
, returnShellEnv ? false
}:
let
  builder =
    hsPkgs.developPackage {
      name = "effective-haskell.com";
      root = pkgs.nix-gitignore.gitignoreSourcePure
        [ "dist-newstyle"
          ".*#"
          ".git"
        ] ./.;
      modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs:
        let
          prevTools = attrs.buildTools or [];
          extraTools = addPkgs pkgs hsPkgs;
          requiredTools = [pkgs.linkchecker hsPkgs.fourmolu hsPkgs.ghcid];
        in {buildTools = prevTools ++ extraTools ++ requiredTools;});
      inherit returnShellEnv;
    };
in builder.overrideAttrs (old: {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LC_ALL = "C.UTF-8";
})
