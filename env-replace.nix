{ pkgs ? import <nixpkgs> {} } :
let deps = import ./dependencies.nix {};
in
  derivation ({
    name        = "trackpad";
    builder     = "${pkgs.bash}/bin/bash";
    system      = builtins.currentSystem;
    env_replace = "${pkgs.env-replace}/bin/env-replace";
  } // deps)
