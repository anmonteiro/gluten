{ ocamlVersion ? "4_11"
, pkgs ? import ./sources.nix { inherit ocamlVersion; }
, doCheck ? true }:

let
  inherit (pkgs) lib stdenv ocamlPackages;

in

  with ocamlPackages;

  let
    genSrc = { dirs, files }: lib.filterGitSource {
      src = ./..;
      inherit dirs;
      files = files ++ [ "dune-project" ];
    };
    buildGluten = args: buildDunePackage ({
      version = "0.1.0-dev";
      useDune2 = true;
      doCheck = false;
    } // args);

    glutenPackages = rec {
      gluten = buildGluten {
        pname = "gluten";
        src = genSrc {
          dirs = [ "lib" ];
          files = [ "gluten.opam" ];
        };
        propagatedBuildInputs = [ bigstringaf faraday ];
      };

      gluten-lwt = buildGluten {
        pname = "gluten-lwt";
        src = genSrc {
          dirs = [ "lwt" ];
          files = [ "gluten-lwt.opam" ];
        };
        propagatedBuildInputs = [ gluten lwt ];
      };

      gluten-lwt-unix = buildGluten {
        pname = "gluten-lwt-unix";
        src = genSrc {
          dirs = [ "lwt-unix" ];
          files = [ "gluten-lwt-unix.opam" ];
        };
        propagatedBuildInputs = [
          faraday-lwt-unix
          gluten-lwt
          lwt_ssl
        ];
      };
    };
  in
  glutenPackages // (if (lib.versionOlder "4.08" ocaml.version) then {
    gluten-async = buildGluten {
      pname = "gluten-async";
      src = genSrc {
        dirs = [ "async" ];
        files = [ "gluten-async.opam" ];
      };
      propagatedBuildInputs = with glutenPackages; [
        faraday-async
        gluten
        async_ssl
      ];
    };

    gluten-mirage = buildGluten {
      pname = "gluten-mirage";
      src = genSrc {
        dirs = [ "mirage" ];
        files = [ "gluten-mirage.opam" ];
      };
      propagatedBuildInputs = with glutenPackages; [
        faraday-lwt
        gluten-lwt
        conduit-mirage
        mirage-flow
        cstruct
      ];
    };
  } else {})
