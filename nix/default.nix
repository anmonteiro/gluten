{ nix-filter, lib, stdenv, ocamlPackages, doCheck ? true }:

with ocamlPackages;

let
  genSrc = { dirs, files }:
    with nix-filter; filter {
      root = ./..;
      include = [ "dune-project" ] ++ files ++ (builtins.map inDirectory dirs);
    };

  buildGluten = args: buildDunePackage ({
    version = "0.1.0-dev";
    useDune2 = true;
    doCheck = false;
  } // args);

  glutenPkgs = rec {
    gluten = buildGluten {
      pname = "gluten";
      src = genSrc {
        dirs = [ "lib" ];
        files = [ "gluten.opam" ];
      };
      propagatedBuildInputs = [ bigstringaf faraday ke ];
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

    gluten-async = buildGluten {
      pname = "gluten-async";
      src = genSrc {
        dirs = [ "async" ];
        files = [ "gluten-async.opam" ];
      };
      propagatedBuildInputs = [
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
      propagatedBuildInputs = [
        faraday-lwt
        gluten-lwt
        conduit-mirage
        mirage-flow
        cstruct
      ];
    };
  };
in

with glutenPkgs;

glutenPkgs // (if lib.versionOlder "5.0" ocaml.version then {
  gluten-eio = buildGluten {
    pname = "gluten-eio";
    src = genSrc {
      dirs = [ "eio" ];
      files = [ "gluten-eio.opam" ];
    };

    propagatedBuildInputs = [
      gluten
      eio
      eio_main
      eio-ssl
    ];
  };

} else { })
