{
  description = "Gluten Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_4_13;
        });
      in
      rec {
        packages = pkgs.callPackage ./nix { inherit pkgs; };
        defaultPackage = packages.gluten;
        devShell = pkgs.callPackage ./shell.nix { inherit pkgs; };
      });
}
