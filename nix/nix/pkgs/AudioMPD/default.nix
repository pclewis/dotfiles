{ stdenv, fetchurl, perlPackages }:

let

  AudioMPDCommon = perlPackages.buildPerlModule {
    name = "Audio-MPD-Common-2.003";
    src = fetchurl {
      url = mirror://cpan/authors/id/J/JQ/JQUELIN/Audio-MPD-Common-2.003.tar.gz;
      sha256 = "d1b59cd678bb149c2591587b48f4a525b2b40d3b5c047ddec8a9f57cfedab6a4";
    };
    buildInputs = [ ];
    propagatedBuildInputs = with perlPackages; [ Moose MooseXHasSugar MooseXTypes Readonly StringFormatter ];
    meta = {
      homepage = http://search.cpan.org/dist/Audio-MPD-Common/;
      description = "Common helper classes for mpd";
      license = "perl";
    };
  };

  GetoptEuclid = perlPackages.buildPerlPackage {
    name = "Getopt-Euclid-0.4.5";
    src = fetchurl {
      url = mirror://cpan/authors/id/F/FA/FANGLY/Getopt-Euclid-0.4.5.tar.gz;
      sha256 = "3cd29f8803fbafefe61b5f8bd6400d3dedc9f3013de451892daed15bd05ffdae";
    };
    meta = {
      homepage = http://search.cpan.org/search?query=Getopt%3A%3AEuclid&mode=dist;
      description = "Executable Uniform Command-Line Interface Descriptions";
      license = "perl";
    };
  };

  ProcProcessTable = perlPackages.buildPerlPackage {
    name = "Proc-ProcessTable-0.53";
    src = fetchurl {
      url = mirror://cpan/authors/id/J/JW/JWB/Proc-ProcessTable-0.53.tar.gz;
      sha256 = "b05811cffb366b1db6f71f2dcbffac5d2cbf01d987f0c0242b671e447326ba44";
    };
    meta = {
      description = "Perl extension to access the unix process table";
      license = "perl";
    };
  };

  ProcDaemon = perlPackages.buildPerlPackage {
    name = "Proc-Daemon-0.23";
    src = fetchurl {
      url = mirror://cpan/authors/id/A/AK/AKREAL/Proc-Daemon-0.23.tar.gz;
      sha256 = "34c0b85b7948b431cbabc97cee580835e515ccf43badbd8339eb109474089b69";
    };
    buildInputs = [ ProcProcessTable ];
    meta = {
      homepage = https://github.com/akreal/Proc-Daemon;
      description = "Run Perl program(s) as a daemon process";
      license = "perl";
    };
  };

  AudioMPD = perlPackages.buildPerlModule {
    name = "Audio-MPD-2.000";
    src = fetchurl {
      url = mirror://cpan/authors/id/J/JQ/JQUELIN/Audio-MPD-2.000.tar.gz;
      sha256 = "7b87c65687e0b18d1278ccb91602d32b259051cde32fd831b8504ff9da425122";
    };
    buildInputs = with perlPackages; [ ListAllUtils ];
    propagatedBuildInputs = with perlPackages; [ AudioMPDCommon DBFile Moose MooseXHasSugar MooseXSemiAffordanceAccessor GetoptEuclid ProcDaemon];
    # Tests depend on external commands like "netstat" and connect to running MPD daemon (!)
    doCheck = false;
    meta = {
      homepage = http://search.cpan.org/dist/Audio-MPD/;
      description = "Class to talk to MPD (Music Player Daemon) servers";
      license = "perl";
    };
  };

in

  AudioMPD

