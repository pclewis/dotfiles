   with import <nixpkgs> {}; {
     pyEnv = stdenv.mkDerivation {
       name = "py";
       buildInputs = [ stdenv python27Full python27Packages.virtualenv python27Packages.xlib ];
     };
   }
