{ pkgs ? import ./sources.nix { inherit ocamlVersion; }
, ocamlVersion ? "4_10"
, doCheck ? true }:

let
  inherit (pkgs) lib stdenv ocamlPackages;

in

  with ocamlPackages;

  let
    buildGluten = args: buildDunePackage ({
      version = "0.1.0-dev";
      useDune2 = true;
      doCheck = false;
      src = lib.gitignoreSource ./..;
    } // args);

    glutenPackages = rec {
      gluten = buildGluten {
        pname = "gluten";
        propagatedBuildInputs = [ bigstringaf faraday ];
      };

      gluten-lwt = buildGluten {
        pname = "gluten-lwt";
        propagatedBuildInputs = [ gluten lwt4 ];
      };

      gluten-lwt-unix = buildGluten {
        pname = "gluten-lwt-unix";
        propagatedBuildInputs = [
          faraday-lwt-unix
          gluten-lwt
          lwt_ssl
        ];
      };
    };
  in
  glutenPackages // (if (lib.versionOlder "4.08" ocaml.version) then {
    gluten-mirage = buildGluten {
      pname = "gluten-mirage";
      propagatedBuildInputs = with glutenPackages; [
        faraday-lwt
        gluten-lwt
        conduit-mirage
        mirage-flow
        cstruct
      ];
    };
  } else {})
