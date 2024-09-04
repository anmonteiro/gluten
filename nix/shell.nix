{ mkShell
, cacert
, curl
, ocamlPackages
, git
, stdenv
, lib
, packages
, release-mode ? false
}:

mkShell {
  inputsFrom = lib.attrValues (lib.filterAttrs (_: value: lib.isDerivation value) packages);
  buildInputs =
    (if release-mode then [
      cacert
      curl
      ocamlPackages.dune-release
      git
    ] else [ ]) ++ (with ocamlPackages; [ merlin utop ocamlformat ]);
}
