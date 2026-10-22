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
        tls-async
      ];
    };
  };
in

glutenPkgs
