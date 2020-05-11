{ ocamlVersion ? "4_10" }:

let
  # overlays = /Users/anmonteiro/Documents/github/nix-overlays;
  overlays =
    builtins.fetchTarball
      https://github.com/anmonteiro/nix-overlays/archive/4c0be8e8.tar.gz;

in

  import "${overlays}/sources.nix" {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});
      })
    ];
  }
