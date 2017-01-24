{ stdenv, fetchFromGitHub, pythonPackages }:

pythonPackages.buildPythonApplication rec {
  name = "selfspy-${version}";

  version = "0.3.0-git-a98a9c0";

  src = fetchFromGitHub {
    owner = "gurgeh";
    repo = "selfspy";
    rev = "a98a9c06d14b6f8b6ad76948ab75a2f09c89c578";
    sha256 = "0d0qvq7s229pcdp11bpwgganj94yh6c22s8qyb0gb7vcn3dydxb3";
  };

  patches = [ ./0001-support-pycryptodome.patch ];

  propagatedBuildInputs = with pythonPackages; [ xlib tkinter sqlalchemy lockfile pycrypto keyring pysqlite ];

  #doCheck = false;

  meta = with stdenv.lib; {
    homepage = https://github.com/gurgeh/selfspy;
    description = "Log everything you do on the computer, for statistics, future reference and all-around fun! ";
    license = licenses.gpl3;
    maintainers = [];
  };
}
