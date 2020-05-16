{ release-mode ? false }:

let
  pkgs = import ./nix/sources.nix {};
  inherit (pkgs) stdenv lib;
  glutenPkgs = pkgs.recurseIntoAttrs (import ./nix { inherit pkgs; doCheck = false; });
  glutenDrvs = lib.filterAttrs (_: value: lib.isDerivation value) glutenPkgs;

in
  with pkgs;

  (mkShell {
    inputsFrom = lib.attrValues glutenDrvs;
    buildInputs =
      (if release-mode then [
        cacert
        curl
        ocamlPackages.dune-release
        git
        opam
      ] else [])
      ++ (with ocamlPackages; [ merlin utop ocamlformat ]);
  }).overrideAttrs (o : {
    propagatedBuildInputs = lib.filter
    (drv:
    !(lib.hasAttr "pname" drv) ||
    drv.pname == null ||
    !(lib.any (name: name == drv.pname) (lib.attrNames glutenDrvs)))
      o.propagatedBuildInputs;
  })

