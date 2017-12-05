#!/bin/bash

echo cabal2nix . '>' cabal.nix
cabal2nix . > cabal.nix
for i in $( find proto -type f -name \*.hs -printf '%P\n' ); do
  echo nix-shell --command '${env_replace}' '<' proto/$i '>' src/$i
  nix-shell --command '${env_replace}' < proto/$i > src/$i
done
