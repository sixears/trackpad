{ mkDerivation, base, data-default, fluffy, path, proclib, stdenv
}:
mkDerivation {
  pname = "trackpad";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base path ];
  executableHaskellDepends = [ base data-default fluffy proclib ];
  description = "manage laptop trackpad";
  license = stdenv.lib.licenses.mit;
}
