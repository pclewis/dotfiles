{ stdenv, fetchurl, pythonPackages, mopidy }:

pythonPackages.buildPythonApplication rec {
  name = "Mopidy-Scrobbler-${version}";

  version = "1.1.1";

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/M/Mopidy-Scrobbler/Mopidy-Scrobbler-${version}.tar.gz";
    md5 = "a10dc37ebf8604c33e4f5ef918807c48";
  };

  propagatedBuildInputs = with pythonPackages; [ mopidy configobj pylast ];

  doCheck = false;

  meta = with stdenv.lib; {
    homepage = https://github.com/mopidy/mopidy-scrobbler;
    description = "Mopidy extension for scrobbling played tracks to Last.fm";
    license = licenses.asl20;
    maintainers = [];
  };
}
