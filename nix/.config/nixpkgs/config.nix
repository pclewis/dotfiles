{

  allowUnfree = true;

  packageOverrides = originalPackages:
  let
    newPackages = originalPackages.pkgs;
    pclPackages = import <pclpkgs> {};
  in {

    #-----------#
    # Use jdk11 #
    #-----------#
    jdk = newPackages.jdk11;

    # except for libreoffice
    libreoffice-args = originalPackages.libreoffice-args // { jdk = originalPackages.jdk; };

    #----#
    # hy #
    #----#
    #
    # use python3, use version 0.12.1
    # `doCheck = false` doesn't seem to work with override, so just redefine the
    # whole thing.
    #
    hy = originalPackages.python35Packages.buildPythonApplication rec {
      name = "hy-${version}";
      version = "0.12.1";
      src = originalPackages.fetchurl {
        url = "mirror://pypi/h/hy/${name}.tar.gz";
        sha256 = "1fjip998k336r26i1gpri18syvfjg7z46wng1n58dmc238wm53sx";
      };
      propagatedBuildInputs = with originalPackages.python35Packages; [ appdirs clint astor rply ];
      doCheck = false;
    };

    #----------------#
    # notify-desktop #
    #----------------#
    #
    notify-desktop = originalPackages.stdenv.mkDerivation rec {
      name = "notify-desktop-${version}";
      version = "0.2.0-9863919";
      src = originalPackages.fetchFromGitHub {
        owner = "nowrep";
        repo = "notify-desktop";
        rev = "9863919fb4ce7820810ac14a09a46ee73c3d56cc";
        sha256 = "1brcvl2fx0yzxj9mc8hzfl32zdka1f1bxpzsclcsjplyakyinr1a";
      };

      postPatch = ''substituteInPlace src/Makefile --replace "/usr/bin" "$out/bin"'';
      preInstall = ''mkdir -p $out/bin'';

      buildInputs = [originalPackages.pkgconfig originalPackages.dbus];
    };

    #--------#
    # deepms #
    #--------#
    #
    ddccontrol = originalPackages.ddccontrol.overrideAttrs( origAttrs: rec {
      postInstall = ''
        cp src/config.h $out/include/ddccontrol/config.h
      '';
    });

    deepms = originalPackages.stdenv.mkDerivation rec {
      name = "deepms-${version}";
      version = "0.0.1-76b87bb";
      src = originalPackages.fetchFromGitHub {
        owner = "pitkley";
        repo = "deepms";
        rev = "76b87bbb83b293d1ec570120a3a29f0b4dc76b23";
        sha256 = "1hi44gs83bx8g3597l7aib5xxljmxszjwfl1k91kca80hksq8kxm";
      };

      postPatch = ''
        echo -e '\nINSTALL(TARGETS deepms DESTINATION ''${CMAKE_INSTALL_PREFIX}/bin)' >> CMakeLists.txt
      '';
      buildInputs = with originalPackages; [ cmake newPackages.ddccontrol libxml2 x11 xorg.libXext ];
    };

    #-------------#
    # opencv-java #
    #-------------#
    opencv-java = (originalPackages.opencv3.override {
        enableFfmpeg = true;
        enableContrib = true;
    }).overrideAttrs( origAttrs: rec {
        name = "${origAttrs.name}-java";
        cmakeFlags = origAttrs.cmakeFlags ++ ["-DBUILD_SHARED_LIBS=OFF"];
        buildInputs = origAttrs.buildInputs ++ [ originalPackages.ant originalPackages.pythonPackages.python ];
        propagatedBuildInputs = origAttrs.propagatedBuildInputs ++ [ newPackages.jdk ];
    });


    #----------------------------------------#
    # Packages installed in user environment #
    #----------------------------------------#
    #
    # update with:
    #   nix-env -uA --always nixos.pclenv
    #
    # sort with:
    #   v i [ SPC x l s
    #
    pclenv = with newPackages; buildEnv {
      name = "pclenv";

      # Sometimes docs are under -man, -doc, or -info dirs in /nix/store, which
      # seems to translate to attrs on the package. I don't know why or if there
      # is a better way to do this. Installing the package with `nix-env -i` or
      # even `nix-shell -p` usually includes the docs.
      paths = let
        catMeMaybe = x: attr: if builtins.hasAttr attr x then [(builtins.getAttr attr x)] else [];
        sections = ["man" "doc" "info"];
      in lib.concatMap (x: [x] ++ (lib.concatMap (catMeMaybe x) sections)) [
        acpi
        ag
        anki
        compton
        direnv
        dmenu2
        docker
        dos2unix
        dtrx
        dunst
        dzen2
        emacs
        feh
        file
        font-manager
        git
        gnupg
        hsetroot
        htop
        hy
        imagemagick
        iotop
        jdk
        jq
        lastpass-cli
        leiningen
        libnotify
        lsof
        mercurial
        mpv
        mu
        ncmpcpp
        nmap
        notify-desktop
        offlineimap
        p7zip
        pass
        patchelf
        pinentry
        pstree
        pwgen
        python3
        qutebrowser
        radare2
        rdesktop
        rofi
        ruby
        rxvt_unicode
        scrot
        sshfs-fuse
        sshuttle
        stow
        synergy
        tcpdump
        traceroute
        unclutter
        unzip
        virtualbox
        whois
        wmctrl
        x11_ssh_askpass
        xclip
        xcompmgr
        xdotool
        xlibs.xdpyinfo
        xscreensaver
        xorg.xwininfo
        zip
      ];
    };
  };
}
