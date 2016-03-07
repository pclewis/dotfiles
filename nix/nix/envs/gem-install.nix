with import <nixpkgs> {}; {
  gemEnv = stdenv.mkDerivation {
    name = "gem-install";
    buildInputs = [ stdenv ruby zlib libxml2 libxslt ];
  };
}
