{
  allowUnfree = true;

  packageOverrides = pkgs: {

    ## Enable XWidgets support in emacs, because why not
    #
    emacs = pkgs.emacs.override { withGTK2=false; withGTK3=true; withXwidgets=true; };

    ## Packages installed in user environment
    #
    # update with:
    #   nix-env -u -A --always nixos.pclenv
    #
    # sort with:
    #   v i [ SPC x l s
    #
    pclenv = with pkgs; buildEnv {
      name = "pclenv";
      paths = [
        acpi
        davmail
        docker
        dos2unix
        dtrx
        dunst
        dzen2
        emacs
        gnupg
        htop
        imagemagick
        jq
        lastpass-cli
        leiningen
        libnotify
        lsof
        mpv
        mu
        ncmpcpp
        nmap
        offlineimap
        p7zip
        pwgen
        qutebrowser
        rdesktop
        scrot
        tcpdump
        traceroute
        unclutter
        xclip
      ];
    };

  };
}
