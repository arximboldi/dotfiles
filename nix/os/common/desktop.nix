{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> { config={allowUnfree=true;}; };

  # the main program I use with wine is Emule and it has been reported
  # to work better with Wine 6... with current wine it hangs/freezes
  # sometimes.
  nixos-wine6 = import (
    let rev = "70e9780f978a80fd5f9d041ad3172b2525f788f7";
    in builtins.fetchTarball rec {
      name   = "nixpkgs-${rev}";
      url    = "https://github.com/arximboldi/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "1wpjp9brpis5lnyip912g2dkr4jgykq3wf31kk8091r1f1pgvh9m";
    }
  ) {};

  arximboldi-overlay = self: super: {
    mpd = super.mpd.overrideAttrs {
      src = super.fetchFromGitHub {
        owner  = "MusicPlayerDaemon";
        repo   = "mpd";
        rev    = "v0.24.4";
        sha256 = "sha256-wiQa6YtaD9/BZsC9trEIZyLcIs72kzuP99O4QVP15nQ=";
      };
    };

    # fix broken for now
    dracula-icon-theme = super.dracula-icon-theme.overrideAttrs {
      src = super.fetchFromGitHub {
        owner  = "Blazin64";
        repo   = "dracula-icons";
        rev    = "5739ffc6578a18d8287744eddc8550551e6e72f4";
        sha256 = "sha256-9v4UCvywUJrESmBt+gJUeR3a0UpnbMD3zQVvibtqgyk=";
      };
    };

    # Optimize rubberband as much as possible... it seems to really ba
    # slow library not sure what more we can do about this...
    # rubberband = unstable.rubberband.overrideAttrs (attrs: {
    #   NIX_ENFORCE_NO_NATIVE = false;
    #   mesonFlags = ["-Dtests=disabled"
    #                 "--buildtype=release"
    #                 "--optimization=3"
    #                 "-Dcpp_args='-march=native'"];
    #   hardeningDisable = [ "all" ];
    # });

    # Compile Mixxx using a PortAudio build that supports JACK
    # Overriding PortAudio globally causes an expensive rebuild I want
    # to avoid until the change is merged upstream...
    # https://github.com/NixOS/nixpkgs/pull/157561
    # mixxx = (unstable.mixxx.override {
    #   rubberband = self.rubberband;
    # }).overrideAttrs (attrs: {
    #   NIX_ENFORCE_NO_NATIVE = false;
    #   cmakeFlags = attrs.cmakeFlags ++ ["-DOPTIMIZE=native"];
    #   hardeningDisable = [ "all" ];
    # });

    covergrid = with super;  python3Packages.buildPythonApplication rec {
      pname = "covergrid";
      version = "2.1.12";
      src = fetchGit {
        url = "https://gitlab.com/coderkun/mcg.git";
        rev = "17fe4ee8cad2265e0283f33be40508561687cddb";
      };
      postInstall = ''
         cp -r data $out/lib/python3.8/site-packages/mcg/
         glib-compile-schemas $out/share/glib-2.0/schemas
      '';
      nativeBuildInputs = [ glib wrapGAppsHook ];
      buildInputs = [ glib gtk3 gobject-introspection ];
      propagatedBuildInputs = with python3Packages; [
        glib gtk3
        pygobject3
        keyring
        avahi
      ];
      strictDeps = false;
    };

    xdotool-arximboldi = with super; xdotool.overrideDerivation (attrs: rec {
      name = "xdotool-${version}";
      version = "git";
      src = fetchFromGitHub {
        owner = "arximboldi";
        repo = "xdotool";
        rev = "61ac3d0bad281e94a5d7b33316a72d48444aa60d";
        sha256 = "198944p7bndxbv41wrgjdkkrwnvddhk8dx6ldk0mad6c8p5gjdk1";
      };
    });

    telegram-alias = pkg: self.runCommand "telegram-alias" {} ''
      mkdir -p $out/bin
      ln -s ${pkg}/bin/telegram-desktop $out/bin/telegram
    '';

    cantata-wrapper = self.writeScriptBin "cantata" ''
      ${self.cantata}/bin/cantata
    '';

    my-emacs = self.emacs-gtk.override {
      withNativeCompilation = true;
      withTreeSitter = true;
    };

    my-emacs-with-packages = (self.emacsPackagesFor self.my-emacs).emacsWithPackages (ps: with ps; [
      treesit-grammars.with-all-grammars
    ]);

    beetcamp = (self.python3Packages.buildPythonApplication {
      pname = "beets-beetcamp";
      version = "0.21.0";
      src = self.fetchFromGitHub {
        repo = "beetcamp";
        owner = "snejus";
        rev = "64c7afc9d87682fb2b7c9f2deb76525e44afb248";
        sha256 = "sha256-d0yvOyfxPPBUpoO6HCWfMq2vVw+CcQo16hx+JRDMkBw=";
      };
      format = "pyproject";
      buildInputs = with self.python3Packages; [ poetry-core ];
      propagatedBuildInputs = with self.python3Packages; [
        setuptools requests cached-property pycountry dateutil ordered-set
      ];
      checkInputs = with self.python3Packages; [
        # pytestCheckHook
        pytest-cov
        pytest-randomly
        pytest-lazy-fixture
        rich
        tox
        types-setuptools
        types-requests
      ] ++ [
        self.beets
      ];
      meta = {
        homepage = "https://github.com/snejus/beetcamp";
        description = "Bandcamp autotagger plugin for beets.";
        license = self.lib.licenses.gpl2;
        inherit (self.beets.meta) platforms;
        maintainers = with self.lib.maintainers; [ rrix ];
      };
    });

    my-beets = (self.beets.override {
      pluginOverrides = {
        bandcamp = {
          enable = true;
          propagatedBuildInputs = [ self.beetcamp ];
        };
      };
    });

    # sidequest-latest = super.sidequest.overrideDerivation (old: rec {
    #   version = "0.10.18";
    #   src = super.fetchurl {
		# 		url = "https://github.com/SideQuestVR/SideQuest/releases/download/v0.10.18/SideQuest-0.10.18.tar.xz";
    #     sha256 = "1dcn2kqcix48xb87185y5gxl2zkw450qjsfj6snm77y4ici5icwj";
		# 	};
    # });
  };

in
{
  # chromecast support
  networking.firewall.allowedTCPPorts = [ 8010 ];

  i18n.extraLocaleSettings = {
    LC_TIME = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
  };

  nixpkgs.config.permittedInsecurePackages = [
    "olm-3.2.16"
    "openssl-1.1.1u"
    "python-2.7.18.6"
    "python2.7-pyjwt-1.7.1"
    "libdwarf-20181024"
    "python2.7-certifi-2021.10.8"
  ];

  # enable virt-manager
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = ["your_username"];
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  # make bazel be happy
  # services.envfs.enable = true;
  # system.activationScripts.binbash = {
  #   deps = [ "binsh" ];
  #   text = ''
  #        ln -s /bin/sh /bin/bash
  #   '';
  # };

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ arximboldi-overlay ];

  services.udev.packages = [
    pkgs.android-udev-rules
  ];
  programs.adb.enable = true;

  # compiling remotely
  services.distccd = {
    enable = true;
    zeroconf = true;
    openFirewall = true;
    logLevel = "info";
    stats.enable = true;
    # bassically --allow-private
    # https://github.com/distcc/distcc/blob/66bf4c56f6af2243c48748139c078f4f01cd639b/src/dopt.c#L134C46-L141C57
    allowedClients = [
      "192.168.0.0/16"
      "10.0.0.0/8"
      "172.16.0.0/12"
      "127.0.0.0/8"
      "fe80::/10"
      "fc00::/7"
      "::1/128"
    ];
  };
  systemd.services.distccd.environment = {
    LISTENER = "::"; # for ipv6 support
    DISTCCD_PATH = builtins.concatStringsSep ":" ["${pkgs.gcc}/bin"];
  };
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    #nssmdns6 = true;
    openFirewall = true;
    publish.enable = true;
    publish.userServices = true;
  };


  # syncthing
  services.syncthing = {
    enable = true;
    user = "raskolnikov";
    dataDir = "/home/raskolnikov/sync";    # Default folder for new synced folders
    configDir = "/home/raskolnikov/.config/syncthing";
  };


  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # programming
    nix-index
    zile
    emacs-pgtk
    # my-emacs-with-packages
    vscode
    gitAndTools.gitFull
    gitAndTools.gh
    git-annex
    git-annex-remote-googledrive
    #git-annex-remote-rclone
    #git-annex-remote-dbx
    git-annex-metadata-gui
    rclone
    git-lfs
    mercurialFull
    ((python3.withPackages (ps: with ps; [
      ipython
      livereload
      # pafy
      pyliblo3
      twilio
      inquirer
      lxml
      tabulate
      subliminal
    ])).override (args: { ignoreCollisions = true; }))
    clipgrab
    ruby
    gcc
    gnumake
    ninja
    icu
    clang
    llvm
    cmake
    docker
    ycmd
    silver-searcher
    gdb
    wireshark
    zeal
    mmv
    #bazel
    google-cloud-sdk
    nodejs
    puredata
    wget
    magic-wormhole
    cached-nix-shell
    unstable.cachix
    unzip
    unrar
    android-tools
    hugo
    niv

    # ai stuff
    # unstable.aider-chat-full
    unstable.aider-chat
    unstable.plandex
    unstable.claude-code
    unstable.gemini-cli
    copilot-language-server
    github-copilot-cli
    gh-copilot
    xsel
    jq

    tabnine
    bear
    clang-tools
    rustfmt
    # nodePackages.standard
    nodePackages.prettier
    cmake-format
    alejandra

    gnomeExtensions.x11-gestures
    gnomeExtensions.tactile
    gnomeExtensions.reorder-workspaces
    gnomeExtensions.syncthing-toggle
    gnomeExtensions.astra-monitor
    gnomeExtensions.xwayland-indicator
    #gnomeExtensions.syncthing-indicator
    #gnomeExtensions.current-screen-only-for-alternate-tab
    touchegg

    linuxPackages.perf
    hotspot
    sysprof
    valgrind
    #kcachegrind

    pandoc
    ispell
    texlive.combined.scheme-medium
    librsvg

    # internet
    # flashplayer
    wirelesstools
    iw
    thunderbird
    transmission_4-gtk
    tor-browser
    firefox
    chromium
    google-chrome
    (pidgin.override {
      plugins = [
        pidgin-otr
        purple-facebook
        purple-hangouts
        purple-matrix
        #telegram-purple
      ];
    })
    # station
    # franz
    # rambox
    # unstable.teams
    # unstable.skypeforlinux
    unstable.slack
    unstable.teams-for-linux
    unstable.discord
    unstable.webcord
    unstable.telegram-desktop
    (telegram-alias unstable.telegram-desktop)
    wasistlos
    zapzap
    signal-desktop
    signal-cli

    yt-dlp
    soulseekqt
    qt5.qtbase
    zoom-us
    polari
    # tdesktop
    signal-desktop
    # wire-desktop
    obs-studio

    # mail
    unstable.notmuch
    isync
    afew
    notify-desktop
    gnupg
    msmtp
    evolution

    keepassxc
    pam_u2f

    # media
    smplayer
    mpv
    mplayer
    vlc
    audacious
    ffmpeg-full
    mkvtoolnix
    mpd
    my-beets
    #cantata-wrapper
    cantata
    amberol
    #gmpc
    plattenalbum
    ymuse
    #covergrid
    ncmpc
    ncmpcpp
    ario
    sonata
    #clerk
    mmtc
    mpc_cli
    mpdris2
    mpdas
    calibre
    qjackctl
    jack2
    #cheese
    sound-juicer
    soundconverter
    lame
    audacity
    gthumb
    gpodder
    amule
    #anbox
    subberthehut
    subdl
    easytag
    picard
    vokoscreen

    # music
    unstable.mixxx
    helm
    vkeybd
    vmpk
    bitwig-studio
    tmsu

    # editing
    gimp-with-plugins
    krita
    inkscape
    # onlyoffice-desktopeditors
    libreoffice-fresh
    onlyoffice-desktopeditors
    xournalpp
    pdftk
    gcolor3
    eyedropper
    blender
    imagemagickBig
    #okular
    poppler_utils
    dia
    figma-linux
    # houdini
    libsForQt5.kruler
    libsForQt5.kmag

    # gaming
    # moonlight-embedded
    moonlight-qt
    #winePackages.stableFull
    #wine64Packages.stagingFull
    #wineWow64Packages.waylandFull
    #winetricks
    nixos-wine6.wine64Packages.stableFull
    nixos-wine6.winetricks
    protontricks
    steam
    steam-run
    sidequest
    scrcpy
    ioquake3
    quake3pointrelease
    quake3e
    openarena
    #alienarena
    superTuxKart
    gnujump
    alephone-marathon
    alephone-durandal
    alephone-infinity
    alephone-red
    gamescope
    # liquidwar
    # liquidwar5
    # unvanquished

    # emulators
    retroarch
    mame
    dosbox
    qemu
    qtemu

    # utils
    # gksu
    tree
    ghex
    gedit
    gnome-terminal
    stow
    usbutils
    trash-cli
    psmisc
    glxinfo
    htop
    ntfs3g
    xorg.xkill
    appimage-run
    lsof
    #virtualbox
    gnome-boxes
    lm_sensors
    stress-ng
    gparted
    exfatprogs
    hfsprogs
    exfat
    exfatprogs
    pv
    progress
    # alarm-clock-applet
    xmagnify
    wallutils
    xwallpaper
    gnome-network-displays
    smartmontools

    # lte internet
    modemmanager
    mobile-broadband-provider-info
    usb-modeswitch
    usb-modeswitch-data
    tailscale

    # desktop
    numix-gtk-theme
    numix-cursor-theme
    numix-icon-theme
    numix-icon-theme-circle
    adementary-theme
    lounge-gtk-theme
    yaru-remix-theme
    stilo-themes
    gradience
    themechanger
    adw-gtk3
    adwaita-qt
    adwaita-qt6
    dracula-theme
    dracula-icon-theme

    taffybar
    feh
    # plasma5.plasma-workspace # for xembedsniproxy
    haskellPackages.status-notifier-item
    rofi
    rofi-mpd
    rofi-top
    rofi-calc
    rofi-bluetooth
    rofi-file-browser
    rofi-emoji
    rofi-pulse-select
    rofi-power-menu
    rofimoji
    #clerk
    emote
    dmenu
    xdotool-arximboldi
    # pa_applet
    # pavucontrol
    pwvucontrol
    qpwgraph
    helvum
    pamixer
    gnome-bluetooth
    syncthing
    libnotify
    system-config-printer
    simplescreenrecorder
    vokoscreen-ng
    guvcview
    fswebcam
    # kdePackages.kamoso

    wmctrl
    dunst
    xorg.xhost
    xvkbd
    xbindkeys
    xautomation
    xmacro
    xsettingsd
    sway
    unetbootin
    picom
    gnome-tweaks
    pango
    #blueman
    blueberry

    # https://github.com/NixOS/nixpkgs/issues/43836#issuecomment-419217138
    hicolor-icon-theme
    gnome-icon-theme
  ];

  programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
  gtk.iconCache.enable = true;

  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      corefonts
      inconsolata
      cantarell-fonts
      jetbrains-mono
      nerd-fonts.jetbrains-mono
      nerd-fonts.zed-mono
      nerd-fonts.victor-mono
      nerd-fonts.iosevka
      ubuntu_font_family
      dejavu_fonts
      noto-fonts
      noto-fonts-emoji
      openmoji-color
      fira
      fira-mono
      fira-code
      fira-code-symbols
      twemoji-color-font
      source-sans-pro
      emojione
      roboto
      roboto-mono
      source-code-pro
      #iosevka
      #iosevka-bin
      helvetica-neue-lt-std
      aileron
    ];
    # fontconfig.localConf = builtins.readFile ./fontconfig.xml;
    # fontconfig.defaultFonts = {
    #   emoji = [ "Noto Color Emoji" ];
    #   monospace = [ "Noto Sans Mono" "emoji" ];
    #   sansSerif = [ "Noto Sans" "emoji" ];
    #   serif = [ "Noto Serif" "emoji" ];
    # };
    # fontconfig.defaultFonts = {
    #   emoji = [ "Noto Color Emoji" ];
    #   monospace = [  ];
    #   sansSerif = [  ];
    #   serif = [  ];
    # };
    # fontconfig.defaultFonts.emoji = ["OpenMoji Color"];
  };

  nixpkgs.config.retroarch = {
    enableNeoCD = true;
    enableFBNeo = true;
    enableFBAlpha2012 = true;
    enableMGBA = false;
    enableMAME = false;
    enableGenesisPlusGX = true;
    enableBeetleSNES = true;
    enableSnes9x = true;
    enableBeetleSaturnHW = true;
    enableBeetlePSXHW = true;
    enableDOSBox = true;
    enableMupen64Plus = true;
    enableParallelN64 = true;
    enableNestopia = true;
  };

  programs.evince.enable = true;
  programs.bash.completion.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;
  # services.tailscale.enable = true;
  services.sysprof.enable = true;
  # services.picom.enable = true;
  # services.picom.vSync = true;

  virtualisation.virtualbox.host.enable = true;
  virtualisation.docker.enable = true;

  # services.blueman.enable = true;
  # services.touchegg.enable = true;

  networking.firewall.enable = false;
  networking.hosts = {
    "163.172.144.97" = ["orion1"];
    "163.172.181.40" = ["orion3"];
    "162.55.168.220" = ["orion4"];
    "78.46.255.228" = ["wendy"];
    "162.55.172.27" = ["laurie"];
    "49.12.219.169" = ["daphne"];
    "162.55.48.161" = ["suzanne"];
    "167.235.96.165" = ["laurel"];
    "142.132.141.13" = ["clara"];
  };

  services.flatpak.enable = true;

  hardware.graphics.enable32Bit = true;
  hardware.bluetooth.enable = true;

  security.rtkit.enable = true;
  # sound.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
    alsa.support32Bit = false;
    jack.enable = false;
  };
  hardware.pulseaudio = {
    enable = false;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
  };
  services.gnome.tinysparql.enable = true;
  services.gnome.localsearch.enable = true;
  # deprecated?
  # xdg.portal.gtkUsePortal = true;

  hardware.openrazer.enable = true;

  programs.sway.enable = false;

  services.pantheon.apps.enable = true;
  # programs.pantheon-tweaks.enable = true;

  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.options = "eurosign:e";
    desktopManager = {
      pantheon.enable = false;
      gnome.enable = true;
      xfce.enable = true;
      #gdm.enable = true;
    };
    displayManager = {
      lightdm.enable = true;
      lightdm.greeters.enso.enable = false;
    };
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      # haskellPackages = pkgs.haskell.packages.ghc8107;
      extraPackages = hs: [hs.taffybar];
    };
  };
  services.displayManager.defaultSession = "none+xmonad";
  services.displayManager.autoLogin = {enable = true; user = "raskolnikov";};
  qt.platformTheme = "gnome";
  xdg.icons.fallbackCursorThemes = ["Yaru-remix" "Adwaita"];

  # add wayland support for slack et al
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  programs.xwayland.enable = true;

  # services.xserver.desktopManager.gnome.flashback.customSessions = [
  #   {
  #     wmName = "xmonad";
  #     wmLabel = "XMonad";
  #     wmCommand = "${pkgs.haskellPackages.xmonad}/bin/xmonad";
  #     enableGnomePanel = false;
  #   }
  # ];

  programs.wireshark.enable = true;
  programs.dconf.enable = true;
  programs.seahorse.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.dbus.packages = [ pkgs.gnome-keyring pkgs.gcr ];
  services.gnome.gnome-keyring.enable = true;

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [];

  services.logind.extraConfig = ''
    RuntimeDirectorySize=4G
  '';
  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=1440
  '';  # 60 * 24 -- one day
}
