
{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> {};

in
{
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/emacs-overlay/archive/b74d4784e7a508f1a4eeec588c4057510ac2fbbbb.tar.gz;
  #   }))
  # ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    mpdevil = with pkgs;  python3Packages.buildPythonApplication rec {
      pname = "mpdevil";
      version = "1.1.1";
      src = fetchGit {
        url = "https://github.com/SoongNoonien/mpdevil.git";
        rev = "7969ec54ffa535924f5d3846e6f82bf200899803";
      };
      nativeBuildInputs = [ glib gtk3 intltool wrapGAppsHook ];
      buildInputs = [ glib gtk3 libnotify pango
                      gsettings-desktop-schemas
                      gobject-introspection
                      python3Packages.distutils_extra ];
      propagatedBuildInputs = with python3Packages; [
        pygobject3
        mpd2
        beautifulsoup4
        requests
      ];
      strictDeps = false;
      postInstall = ''
         glib-compile-schemas $out/share/glib-2.0/schemas
      '';
    };
    covergrid = with pkgs;  python3Packages.buildPythonApplication rec {
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
    xdotool-arximboldi = with pkgs; xdotool.overrideDerivation (attrs: rec {
      name = "xdotool-${version}";
      version = "git";
      src = fetchFromGitHub {
        owner = "arximboldi";
        repo = "xdotool";
        rev = "61ac3d0bad281e94a5d7b33316a72d48444aa60d";
        sha256 = "198944p7bndxbv41wrgjdkkrwnvddhk8dx6ldk0mad6c8p5gjdk1";
      };
    });
    xorg = pkgs.xorg // {
      xf86videointel = pkgs.xorg.xf86videointel.overrideDerivation (old: {
        src = fetchGit {
          url = "https://gitlab.freedesktop.org/xorg/driver/xf86-video-intel.git";
          rev = "31486f40f8e8f8923ca0799aea84b58799754564";
        };
      });
    };
    sidequest-latest = pkgs.sidequest.overrideDerivation (old: rec {
      version = "0.10.18";
      src = pkgs.fetchurl {
				url = "https://github.com/SideQuestVR/SideQuest/releases/download/v0.10.18/SideQuest-0.10.18.tar.xz";
        sha256 = "1dcn2kqcix48xb87185y5gxl2zkw450qjsfj6snm77y4ici5icwj";
			};
    });
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # programming
    zile
    emacs
    gitAndTools.gitFull
    gitAndTools.gh
    git-lfs
    mercurialFull
    ((python3.withPackages (ps: with ps; [
      ipython
      livereload
      pafy
    ])).override (args: { ignoreCollisions = true; }))
    clipgrab
    ruby
    gcc
    clang
    gnumake
    ninja
    icu
    clang-tools
    cmake
    docker
    ycmd
    silver-searcher
    clang-tools
    gdb
    rustfmt
    wireshark
    zeal
    llvm
    mmv
    bazel
    google-cloud-sdk
    nodejs
    puredata
    wget
    magic-wormhole
    cached-nix-shell

    pandoc
    ispell
    texlive.combined.scheme-medium
    gnome.librsvg

    # internet
    # flashplayer
    thunderbird
    transmission-gtk
    unstable.firefox
    unstable.chromium
    unstable.google-chrome
    (pidgin.override {
      plugins = [
        pidgin-otr
        purple-facebook
        purple-hangouts
        purple-matrix
        telegram-purple
      ];
    })
    # station
    # franz
    # rambox
    unstable.skype
    unstable.slack
    soulseekqt
    qt5.qtbase
    zoom-us
    unstable.discord
    gnome3.polari
    tdesktop
    signal-desktop
    wire-desktop
    obs-studio

    # mail
    notmuch
    isync
    afew
    notify-desktop
    gnupg
    msmtp

    # media
    smplayer
    mpv
    mplayer
    vlc
    ffmpeg-full
    mpd
    cantata
    gmpc
    covergrid
    mpdevil
    mpc_cli
    mpdris2
    mpdas
    calibre
    qjackctl
    jack2Full
    gnome3.cheese
    sound-juicer
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
    # houdini

    # gaming
    wineStaging
    winetricks
    protontricks
    steam
    steam-run
    sidequest-latest
    scrcpy
    ioquake3
    quake3pointrelease
    quake3e
    openarena
    alienarena
    superTuxKart
    gnujump
    # liquidwar
    # liquidwar5
    # unvanquished

    # emulators
    retroarch
    mame
    dosbox

    # utils
    gksu
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
    cv
    alarm-clock-applet
    xmagnify
    wallutils

    # lte internet
    modemmanager
    mobile_broadband_provider_info
    usb-modeswitch
    usb-modeswitch-data
    tailscale

    # desktop
    numix-gtk-theme
    numix-cursor-theme
    numix-icon-theme
    numix-icon-theme-circle
    taffybar
    feh
    # plasma5.plasma-workspace # for xembedsniproxy
    haskellPackages.status-notifier-item
    dmenu
    xdotool-arximboldi
    pa_applet
    pasystray
    pavucontrol
    pulseaudio-ctl
    blueman
    gnome3.gnome-bluetooth
    blueberry
    syncthing
    libnotify
    system-config-printer

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
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      inconsolata
      ubuntu_font_family
      dejavu_fonts
      fira
      fira-mono
      fira-code
      fira-code-symbols
      source-sans-pro
      emojione
      roboto
      roboto-mono
      source-code-pro
      iosevka
      iosevka-bin
    ];
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

  programs.bash.enableCompletion = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;
  # services.tailscale.enable = true;
  # services.picom.enable = true;
  # services.picom.vSync = true;

  virtualisation.virtualbox.host.enable = true;
  virtualisation.docker.enable = true;

  services.blueman.enable = true;

  networking.firewall.enable = false;
  networking.hosts = {
    "163.172.144.97" = ["orion1"];
    "163.172.181.40" = ["orion3"];
    "78.46.255.228" = ["wendy"];
    "162.55.172.27" = ["laurie"];
    "49.12.219.169" = ["daphne"];
  };

  sound.enable = true;
  services.flatpak.enable = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.bluetooth.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
  };

  hardware.openrazer.enable = true;

  programs.sway.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";
    displayManager.gdm.enable = false;
    displayManager.lightdm.enable = true;
    displayManager.autoLogin = {enable = true; user = "raskolnikov";};
    displayManager.lightdm.greeters.enso.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager.gnome.enable = true;
    desktopManager.xfce.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hs: [hs.taffybar];
    };
  };

  programs.wireshark.enable = true;
  programs.dconf.enable = true;
  programs.seahorse.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.dbus.packages = [ pkgs.gnome3.gnome-keyring pkgs.gnome3.gcr ];
  services.gnome.gnome-keyring.enable = true;
}
