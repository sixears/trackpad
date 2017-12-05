{ pkgs ? import <nixpkgs> {} }:

with pkgs;
{ xinput = xorg.xinput; }
