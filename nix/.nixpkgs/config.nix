{

  allowUnfree = true;

  packageOverrides = originalPackages: let newPackages = originalPackages.pkgs; in {

    #-------------#
    # qutebrowser #
    #-------------#
    #
    # override changes arguments to derivation (ex options and dependencies)
    # overrideAttrs changes the attrs of the actual derivation (ex version, build steps)
    #
    qutebrowser = (originalPackages.qutebrowser.override {
      ##
      # fix qtwebkit-plugins so notifications and spellcheck work
      ##
      qtwebkit-plugins = originalPackages.qt5.qtwebkit-plugins.overrideAttrs(oldAttrs: {
        postPatch = oldAttrs.postPatch + ''
          sed -i "s,plugin.h,plugin.h ${newPackages.qt5.qtwebkit.dev}/include/QtWebKit/qwebkitplatformplugin.h," src/src.pro  
        '';
      });

      ##
      # qtwebengine
      ##
      pyqt5 = originalPackages.python3Packages.pyqt5.overrideAttrs(oldAttrs: {
        buildInputs = oldAttrs.buildInputs ++ [ originalPackages.qt5.qtwebengine ];
      });

    }).overrideAttrs(oldAttrs: rec {
      ##
      # 0.9.0 -> 0.9.1
      ##
      version = assert oldAttrs.version == "0.9.0"; "0.9.1";
      name = "qutebrowser-${version}";
      src = originalPackages.fetchurl {
        url = "https://github.com/The-Compiler/qutebrowser/releases/download/v${version}/${name}.tar.gz";
        sha256 = "0pf91nc0xcykahc3x7ww525c9czm8zpg80nxl8n2mrzc4ilgvass";
      };
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
        ack
        acpi
        ag
        chromium
        compton
        davmail
        dmenu2
        docker
        dos2unix
        dtrx
        dunst
        dzen2
        emacs
        #firefox-bin
        git
        gnupg
        hsetroot
        htop
        imagemagick
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
        offlineimap
        p7zip
        pinentry
        pwgen
        python3
        qutebrowser
        rdesktop
        ruby
        rxvt_unicode
        scrot
        stow
        tcpdump
        traceroute
        unclutter
        unzip
        x11_ssh_askpass
        xclip
        xcompmgr
        xlibs.xdpyinfo
        xscreensaver
        zip
      ];
    };
  };
}
