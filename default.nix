{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, data-default, fluffy, lens
      , optparse-applicative, path, proclib, stdenv, text
      }:
      mkDerivation {
        pname = "trackpad";
        version = "0.0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base path ];
        executableHaskellDepends = [
          base data-default fluffy lens optparse-applicative proclib text
        ];
        description = "manage laptop trackpad";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
