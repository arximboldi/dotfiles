{ inputs, config, pkgs, ... }:

let
  unstable = import inputs.nixos-unstable {
    system = pkgs.system;
    config = config.nixpkgs.config;
  };

  arximboldi-overlay = self: super: {
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

    polkit-gnome-alias = self.runCommand "polkit-gnome-alias" {} ''
      mkdir -p $out/bin
      ln -s ${self.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1 $out/bin/
    '';

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

    mako = with super; mako.overrideDerivation (attrs: rec {
      name = "mako-${version}";
      version = "arximboldi-git";
      src = fetchFromGitHub {
        owner = "arximboldi";
        repo = "mako";
        rev = "ad04199c40844d50136c255d33a68bffb541496f";
        sha256 = "sha256-x4+J1RigiSlXlmFKJT01FhmdKZPPTeAtymHZiiecBho=";
      };
    });

    hyprland = unstable.hyprland;

    opentabletdriver = unstable.opentabletdriver.overrideAttrs (oldAttrs: rec {
      src = super.fetchFromGitHub {
        owner = "OpenTabletDriver";
        repo = "OpenTabletDriver";
        rev = "0989c15dfe6e1e656af58400b69b691e357dafb6";
        sha256 = "sha256-Vo0ljnbL40YGX52nYXpbfGYWX6cYvX38ZdtcRUhVLgw=";
      };
    });
  };

in
{

  i18n.extraLocaleSettings = {
    LC_TIME = "en_GB.UTF-8";
    LC_MEASUREMENT = "en_GB.UTF-8";
  };


  nixpkgs.overlays = [ arximboldi-overlay ];

  programs.hyprland.enable = true;

  hardware.opentabletdriver.enable = true;

  #xdg.portal = {
  #  enable = true;
  #  config.common.default = [ "hyprland" "gtk" ];
  #  extraPortals = [
  #    pkgs.xdg-desktop-portal-gtk
  #    pkgs.xdg-desktop-portal-hyprland
  #  ];
  #};

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # programming
    puredata

    # ai stuff
    # unstable.aider-chat-full

    gnomeExtensions.x11-gestures
    gnomeExtensions.tactile
    gnomeExtensions.reorder-workspaces
    gnomeExtensions.syncthing-toggle
    gnomeExtensions.astra-monitor
    gnomeExtensions.xwayland-indicator
    # gnomeExtensions.syncthing-indicator
    # gnomeExtensions.current-screen-only-for-alternate-tab
    touchegg

    obs-studio
    dconf-editor

    keepassxc
    pam_u2f

    # media
    smplayer
    mpv
    mplayer
    vlc
    # audacious
    ffmpeg-full
    mkvtoolnix
    calibre
    qjackctl
    jack2
    # cheese
    sound-juicer
    soundconverter
    lame
    audacity
    gthumb
    gpodder
    # anbox
    subberthehut
    subdl
    python3Packages.subliminal
    easytag
    picard
    vokoscreen

    # music
    unstable.mixxx
    helm
    vkeybd
    vmpk
    bitwig-studio

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
    tuhi
    inklingreader
    wacomtablet
    mypaint
    obsidian
    libwacom
    # libwacom-surface
    kdePackages.wacomtablet
    # haskellPackages.wacom-daemon
    pantheon.switchboard
    pantheon.switchboard-plug-wacom
    libinput
    input-remapper
    opentabletdriver
    rnote
    osu-lazer-bin

    # utils
    # gksu
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
    lm_sensors
    stress-ng
    pv
    progress
    # alarm-clock-applet
    xmagnify
    wallutils
    xwallpaper
    gnome-network-displays
    smartmontools

    # partition manager
    gparted
    exfatprogs
    hfsprogs
    exfat
    exfatprogs

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
    darkly
    dracula-theme
    dracula-icon-theme
    kdePackages.qt6ct

    # plasma5.plasma-workspace # for xembedsniproxy
    haskellPackages.status-notifier-item
    # rofi
    (rofi-wayland.override {
      plugins = [
        rofi-calc
        rofi-bluetooth
        rofi-file-browser
        rofi-emoji
        rofi-emoji-wayland
        rofi-top
        rofi-emoji
        rofi-pulse-select
        rofi-power-menu
      ];
    })
    rofi-mpd
    # clerk
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
    libnotify
    system-config-printer
    simplescreenrecorder
    vokoscreen-ng
    guvcview
    fswebcam
    # kdePackages.kamoso
    hyprland
    hyprpanel
    hyprpaper
    hyprpicker
    hyprsunset
    hypridle
    hyprlock
    unstable.sunsetr
    hyprshot
    kooha
    gtklock
    wev

    # unstable.quickshell
    wofi
    wofi-emoji
    wofi-power-menu
    bemenu
    bemoji
    unstable.waybar # fix bug in update layout
    playerctl
    brightnessctl
    wlprop
    swaybg
    mako
    dmenu-wayland
    ydotool
    wtype
    wmctrl
    # dunst
    xorg.xhost
    xvkbd
    xbindkeys
    xautomation
    xmacro
    xsettingsd
    sway
    unetbootin
    # picom
    gnome-tweaks
    pango
    kitty
    gdm-settings
    # blueman
    blueberry
    swayosd
    nwg-displays
    nwg-look
    wlogout
    wlock
    # mess with auto-start
    # picom
    # pasystray
    # mpdris2
    # blueberry
    polkit-gnome-alias
    networkmanagerapplet

    # https://github.com/NixOS/nixpkgs/issues/43836#issuecomment-419217138
    hicolor-icon-theme
    gnome-icon-theme
  ];

  services.input-remapper.enable = true;

  security.pam.services.gtklock = {};

  programs.gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
  gtk.iconCache.enable = true;

  # https://nixos.wiki/wiki/Fonts
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
      helvetica-neue-lt-std
      aileron
    ];
    fontconfig.useEmbeddedBitmaps = true;
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
    pinentryPackage = pkgs.pinentry-gnome3;
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

  services.flatpak.enable = true;

  hardware.graphics.enable32Bit = true;
  hardware.bluetooth.enable = true;

  services.gnome.tinysparql.enable = true;
  services.gnome.localsearch.enable = true;
  # deprecated?
  # xdg.portal.gtkUsePortal = true;

  programs.sway.enable = false;

  services.pantheon.apps.enable = false;
  # programs.pantheon-tweaks.enable = true;

  services.xserver = {
    enable = true;
    xkb.layout = "us";
    xkb.options = "eurosign:e";
    desktopManager = {
      pantheon.enable = false;
      gnome.enable = true;
      xfce.enable = false;
    };
    displayManager = {
      gdm.enable = true;
      #lightdm.enable = true;
      #lightdm.greeters.enso.enable = false;
    };
    windowManager.xmonad = {
      enable = false;
      enableContribAndExtras = true;
      # haskellPackages = pkgs.haskell.packages.ghc8107;
      extraPackages = hs: [hs.taffybar];
    };
  };
  services.displayManager = {
    #sddm = {
    #  enable = true;
    #  wayland.enable = true;
    #  enableHidpi = true;
    #  theme = "chili";
    #};
    # defaultSession = "gnome"; # "none+xmonad"
    # autoLogin = {enable = true; user = "raskolnikov";};
  };

  qt.platformTheme = "gnome";
  xdg.icons.fallbackCursorThemes = ["Adwaita"];

  # add wayland support for slack et al
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  # environment.sessionVariables.QT_QPA_PLATFORM = "wayland";
  programs.xwayland.enable = true;

  # services.xserver.desktopManager.gnome.flashback.customSessions = [
  #   {
  #     wmName = "xmonad";
  #     wmLabel = "XMonad";
  #     wmCommand = "${pkgs.haskellPackages.xmonad}/bin/xmonad";
  #     enableGnomePanel = false;
  #   }
  # ];

  programs.dconf.enable = true;
  programs.seahorse.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  security.pam.services.gdm.enableGnomeKeyring = true;
  security.pam.services.sddm.enableGnomeKeyring = true;
  services.dbus.packages = [ pkgs.gnome-keyring pkgs.gcr ];
  services.gnome.gnome-keyring.enable = true;

  security.pam.u2f.enable = true;
  services.udev.extraRules = ''
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0664", GROUP="plugdev", TAG+="uaccess"
  '';

  # make sure tmp folder is big enough
  services.logind.extraConfig = ''
    RuntimeDirectorySize=4G
  '';

  security.sudo.extraConfig = ''
    Defaults timestamp_timeout=1440
  '';  # 60 * 24 -- one day
}
