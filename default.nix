{ mkDerivation
, stdenv
, base
, haskell-language-server
, reflex
, niv
, time
, random
, cabal-install
, fsnotify
, process
}:

mkDerivation {
  pname = "AutoGitSync";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    reflex
    time
    random
    fsnotify
    process
  ];
  buildTools = [
    cabal-install
    haskell-language-server
    niv
  ];
  license = "MIT";
  hydraPlatforms = stdenv.lib.platforms.none;
}
