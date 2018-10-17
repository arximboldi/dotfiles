# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> {};

  musnixSrc = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "musnix";
    repo   = "musnix";
    rev    = "cec9d0529977e2db2a273f33c3261620098465ed";
    sha256 = "1ybja7i5c8nh0drlp4pjxkp3v6zp7f8hi8d8nwbsgf2ym9cxjlwf";
  };

in
{
  imports = [
    ./hardware-configuration.nix
    <nixpkgs/nixos/modules/services/hardware/sane_extra_backends/brscan4.nix>
    musnixSrc.outPath
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ce1";
  time.timeZone = "Europe/Berlin"; # "America/Vancouver"; # "America/New_York"; # "Europe/Berlin";
  # damn rubygems...
  # networking.enableIPv6 = false;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    xdotool-arximboldi = pkgs.stdenv.mkDerivation rec {
      name = "xdotool-${version}";
      version = "git";
      src = pkgs.fetchFromGitHub {
        owner = "arximboldi";
        repo = "xdotool";
        rev = "61ac3d0bad281e94a5d7b33316a72d48444aa60d";
        sha256 = "198944p7bndxbv41wrgjdkkrwnvddhk8dx6ldk0mad6c8p5gjdk1";
      };
      nativeBuildInputs = with pkgs; [ pkgconfig perl ];
      buildInputs = with pkgs; with xorg; [ libX11 libXtst xextproto libXi libXinerama libxkbcommon ];
      preBuild = ''
        mkdir -p $out/lib
      '';
      makeFlags = "PREFIX=$(out)";
      meta = {
        homepage = http://www.semicomplete.com/projects/xdotool/;
        description = "Fake keyboard/mouse input, window management, and more";
        license = pkgs.stdenv.lib.licenses.bsd3;
        maintainers = with pkgs.stdenv.lib.maintainers; [viric];
        platforms = with pkgs.stdenv.lib.platforms; linux;
      };
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # programming
    zile
    emacs
    gitAndTools.gitFull
    (python.withPackages (ps: with ps; [
      ipython
    ]))
    ruby
    gcc
    gnumake
    icu
    unstable.clang-tools
    cmake

    # internet
    firefox
    chromium
    google-chrome
    pidgin
    pidgin-otr
    unstable.skype
    unstable.slack
    unstable.soulseekqt
    unstable.qt5.full
    gnome3.polari
    unstable.youtube-dl

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
    libvdpau
    libvdpau-va-gl
    ffmpeg-full
    mpd
    cantata
    gmpc
    mpdris2
    calibre
    qjackctl
    jack2Full

    # editing
    gimp-with-plugins
    inkscape
    libreoffice-fresh
    xournal
    pdftk
    gcolor2
    blender
    unstable.shotcut
    unstable.ladspaPlugins
    unstable.kdenlive
    unstable.frei0r
    unstable.breeze-icons

    # gaming
    wineStaging
    # ioquake3
    # unvanquished

    # utils
    stow
    wget
    usbutils
    trash-cli
    psmisc
    glxinfo
    htop
    ntfs3g
    xorg.xkill
    appimage-run
    lsof
    virtualbox

    # desktop
    numix-gtk-theme
    numix-cursor-theme
    numix-icon-theme
    numix-icon-theme-circle
    taffybar
    dmenu
    ibus
    xdotool-arximboldi
    pa_applet
    pavucontrol
    blueman
    syncthing
    libnotify
    system-config-printer
    unstable.brgenml1lpr
    # notify-osd-customizable
    dunst
  ];

  fonts = {
    enableFontDir = true;
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
    ];
  };

  i18n.inputMethod.enabled = "ibus";
  programs.bash.enableCompletion = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;


  services.printing.enable = true;
  services.printing.drivers = [ unstable.brgenml1cupswrapper ];

  hardware.opengl.driSupport32Bit = true;

  hardware.sane = {
    enable = true;
    brscan4 = {
      enable = true;
      netDevices = {
        home = {
          model = "DCP-L2520DW";
          ip = "192.168.0.4";
        };
      };
    };
  };

  musnix.enable = true;
  sound.enable = true;
  nixpkgs.config.pulseaudio = true;
  hardware.bluetooth.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    daemon.config = {
      enable-remixing = "no";
      enable-lfe-remixing = "no";
    };
    extraConfig = ''
      .nofail
      load-module module-remap-sink sink_name=ka6stereo sink_properties="device.description='Komplete Audio 6 Stereo'" remix=no master=alsa_output.usb-Native_Instruments_Komplete_Audio_6_781D08CA-00.analog-surround-50 channels=2 master_channel_map=front-left,front-right channel_map=front-left,front-right
      set-default-sink ka6stereo
      .fail
    '';
  };

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";
    displayManager.gdm.enable = false;
    displayManager.lightdm.enable = true;
    desktopManager.gnome3.enable = true;
    desktopManager.xfce.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hs: [hs.taffybar];
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.raskolnikov = {
     isNormalUser = true;
     uid = 1000;
     extraGroups = [
       "wheel"
       "audio"
       "users"
       "scanner"
       "lp"
       "networkmanager"
     ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
