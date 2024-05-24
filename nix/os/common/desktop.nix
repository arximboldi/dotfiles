{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> {};

  nixos-21_11 = import (
    let rev = "ec6eaba9dfcfdd11547d75a193e91e26701bf7e3";
    in builtins.fetchTarball rec {
      name   = "nixpkgs-${rev}";
      url    = "https://github.com/arximboldi/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "018658ypakpg9yxbyzd06vaxzs5956lr30jkfvw0fzn33pp01xdg";
    }
  ) {};

  arximboldi-overlay = self: super: {
    # Optimize rubberband as much as possible... it seems to really ba
    # slow library not sure what more we can do about this...
    rubberband = super.rubberband.overrideAttrs (attrs: {
      mesonFlags = ["--buildtype=release"
                    "--optimization=3"
                    "-Dcpp_args='-march=native'"];
      makeFlags = attrs.makeFlags ++ ["-v"];
      hardeningDisable = [ "all" ];
    });

    # This causes too much to rebuild if set for everything... We are
    # ok with the library sometimes being less optimized?
    #
    # jack2-opt = super.jack2.overrideAttrs (attrs: {
    #   CFLAGS   = "-O3 -march=native";
    #   CXXFLAGS = "-O3 -march=native";
    #   wafFlags = ["-v"];
    #   hardeningDisable = [ "all" ];
    # });

    # Compile Mixxx using a PortAudio build that supports JACK
    # Overriding PortAudio globally causes an expensive rebuild I want
    # to avoid until the change is merged upstream...
    # https://github.com/NixOS/nixpkgs/pull/157561
    mixxx = (super.mixxx.override {
      rubberband = self.rubberband;
      # portaudio = super.portaudio.overrideAttrs (attrs: {
      #   CFLAGS   = "-O3 -march=native";
      #   CXXFLAGS = "-O3 -march=native";
      #   hardeningDisable = [ "all" ];
      #   buildInputs = attrs.buildInputs ++ [self.jack2-opt];
      # });
    }).overrideAttrs (attrs: {
      cmakeFlags = attrs.cmakeFlags ++ ["-DOPTIMIZE=native"];
      makeFlags = ["VERBOSE=1"];
      hardeningDisable = [ "all" ];
    });

    # mpdevil = with super;  python3Packages.buildPythonApplication rec {
    #   pname = "mpdevil";
    #   version = "1.1.1";
    #   src = fetchGit {
    #     url = "https://github.com/SoongNoonien/mpdevil.git";
    #     rev = "7969ec54ffa535924f5d3846e6f82bf200899803";
    #   };
    #   nativeBuildInputs = [ glib gtk3 intltool wrapGAppsHook ];
    #   buildInputs = [ glib gtk3 libnotify pango
    #                   gsettings-desktop-schemas
    #                   gobject-introspection
    #                   python3Packages.distutils_extra ];
    #   propagatedBuildInputs = with python3Packages; [
    #     pygobject3
    #     mpd2
    #     beautifulsoup4
    #     requests
    #   ];
    #   strictDeps = false;
    #   postInstall = ''
    #      glib-compile-schemas $out/share/glib-2.0/schemas
    #   '';
    # };

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

    telegram-alias = self.runCommand "telegram-kotatogram" {} ''
      mkdir -p $out/bin
      ln -s ${unstable.kotatogram-desktop}/bin/kotatogram-desktop $out/bin/telegram
    '';

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
    "openssl-1.1.1u"
    "python-2.7.18.6"
    "python2.7-pyjwt-1.7.1"
    "libdwarf-20181024"
    "python2.7-certifi-2021.10.8"
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ arximboldi-overlay ];

  services.udev.packages = [
    pkgs.android-udev-rules
  ];
  programs.adb.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # programming
    zile
    emacs29-gtk3
    vscode
    gitAndTools.gitFull
    gitAndTools.gh
    git-lfs
    mercurialFull
    ((unstable.python3.withPackages (ps: with ps; [
      ipython
      livereload
      # pafy
      pyliblo
      twilio
      inquirer
      lxml
      tabulate
    ])).override (args: { ignoreCollisions = true; }))
    clipgrab
    ruby
    gcc
    gnumake
    ninja
    icu
    clang_16
    clang-tools_16
    llvm_16
    cmake
    docker
    ycmd
    silver-searcher
    gdb
    rustfmt
    wireshark
    zeal
    mmv
    bazel
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

    linuxPackages.perf
    hotspot
    sysprof
    valgrind
    kcachegrind

    pandoc
    ispell
    texlive.combined.scheme-medium
    librsvg

    # internet
    # flashplayer
    wirelesstools
    iw
    thunderbird
    transmission-gtk
    tor-browser
    unstable.firefox
    unstable.chromium
    unstable.google-chrome
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
    unstable.skypeforlinux
    unstable.slack
    # unstable.teams
    unstable.teams-for-linux
    soulseekqt
    qt5.qtbase
    zoom-us
    unstable.discord
    gnome3.polari
    # tdesktop
    unstable.kotatogram-desktop
    telegram-alias
    signal-desktop
    wire-desktop
    obs-studio

    # mail
    unstable.notmuch
    isync
    afew
    notify-desktop
    gnupg
    msmtp
    evolution

    # media
    smplayer
    mpv
    mplayer
    vlc
    audacious
    ffmpeg-full
    mpd
    cantata
    gmpc
    #covergrid
    mpdevil
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
    gnome3.cheese
    sound-juicer
    soundconverter
    lame
    audacity
    gthumb
    gpodder
    amule
    anbox
    subberthehut
    subdl
    easytag
    picard
    vokoscreen

    # music
    mixxx
    helm
    vkeybd
    vmpk
    bitwig-studio

    # editing
    gimp-with-plugins
    krita
    inkscape
    libreoffice-fresh
    xournalpp
    pdftk
    gcolor2
    blender
    imagemagickBig
    okular
    poppler_utils
    dia
    figma-linux
    # houdini
    libsForQt5.kruler
    libsForQt5.kmag

    # gaming
    # moonlight-embedded
    moonlight-qt
    wine-staging
    winetricks
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
    gnome.gedit
    gnome.gnome-terminal
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
    lm_sensors
    stress-ng
    gparted
    hfsprogs
    exfat
    pv
    progress
    # alarm-clock-applet
    xmagnify
    wallutils
    xwallpaper
    gnome-network-displays

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
    pa_applet
    pasystray
    pavucontrol
    pamixer
    blueman
    gnome3.gnome-bluetooth
    blueberry
    syncthing
    libnotify
    system-config-printer
    simplescreenrecorder

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
    gnome3.gnome-tweaks
    pango

    # https://github.com/NixOS/nixpkgs/issues/43836#issuecomment-419217138
    hicolor-icon-theme
    gnome-icon-theme
  ];

  services.xserver.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
  gtk.iconCache.enable = true;

  fonts = {
    enableDefaultFonts = true;
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      inconsolata
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
      iosevka
      iosevka-bin
      helvetica-neue-lt-std
      aileron
    ];
    fontconfig.localConf = builtins.readFile ./fontconfig.xml;
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
  programs.bash.enableCompletion = true;
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

  services.blueman.enable = true;

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

  hardware.opengl.driSupport32Bit = true;
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
  services.gnome.tracker.enable = true;
  services.gnome.tracker-miners.enable = true;
  # deprecated?
  # xdg.portal.gtkUsePortal = true;

  hardware.openrazer.enable = true;

  programs.sway.enable = true;

  services.pantheon.apps.enable = true;
  programs.pantheon-tweaks.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";
    desktopManager.pantheon.enable = false;
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = false;
    displayManager.lightdm.enable = true;
    displayManager.autoLogin = {enable = true; user = "raskolnikov";};
    displayManager.lightdm.greeters.enso.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager.xfce.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      # haskellPackages = pkgs.haskell.packages.ghc8107;
      extraPackages = hs: [hs.taffybar];
    };
  };

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
  services.dbus.packages = [ pkgs.gnome3.gnome-keyring pkgs.gcr ];
  services.gnome.gnome-keyring.enable = true;
}
