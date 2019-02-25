# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> {};

  fetchFromGitHub = (import <nixpkgs> {}).fetchFromGitHub;

  nixos-1803 = import (fetchFromGitHub {
    owner  = "nixos";
    repo   = "nixpkgs-channels";
    rev    = "138f2cc707d7ee13d93c86db3285460e244c402c";
    sha256 = "0h49j1cbnccqx996x80z7na9p7slnj9liz646s73s55am8wc9q8q";
  }) {};

  mozilla = import (fetchFromGitHub {
    owner  = "mozilla";
    repo   = "nixpkgs-mozilla";
    rev    = "0d64cf67dfac2ec74b2951a4ba0141bc3e5513e8";
    sha256 = "0ngj2rk898rq73rq2rkwjax9p34mjlh3arj8w9npwwd6ljncarmh";
  });

  musnix-src = fetchFromGitHub {
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
    musnix-src.outPath
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ce1";
  time.timeZone = "Europe/Berlin"; # "America/Vancouver"; # "America/New_York"; # "Europe/Berlin";
  # Damn Rubygems and Bitbucket...
  networking.enableIPv6 = false;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ mozilla ];

  nixpkgs.config.packageOverrides = pkgs: {

    xdotool-arximboldi = with pkgs; stdenv.mkDerivation rec {
      name = "xdotool-${version}";
      version = "git";
      src = fetchFromGitHub {
        owner = "arximboldi";
        repo = "xdotool";
        rev = "61ac3d0bad281e94a5d7b33316a72d48444aa60d";
        sha256 = "198944p7bndxbv41wrgjdkkrwnvddhk8dx6ldk0mad6c8p5gjdk1";
      };
      nativeBuildInputs = [ pkgconfig perl ];
      buildInputs = with xorg; [ libX11 libXtst xextproto libXi libXinerama libxkbcommon ];
      preBuild = ''
        mkdir -p $out/lib
      '';
      makeFlags = "PREFIX=$(out)";
      meta = {
        homepage = http://www.semicomplete.com/projects/xdotool/;
        description = "Fake keyboard/mouse input, window management, and more";
        license = pkgs.stdenv.lib.licenses.bsd3;
        maintainers = with stdenv.lib.maintainers; [viric];
        platforms = with stdenv.lib.platforms; linux;
      };
    };

    mixxx-latest = with pkgs; stdenv.mkDerivation rec {
      name = "mixxx-${version}";
      version = "2.0.0";
      src = fetchFromGitHub {
        owner = "mixxxdj";
        repo = "mixxx";
        rev = "release-${version}";
        sha256 = "0pipmkv5fig2pajlh5nnmxyfil7mv5l86cw6rh8jbkcr9hman9bp";
      };
      nativeBuildInputs = [ scons makeWrapper ];
      buildInputs = [
        chromaprint fftw flac faad2 glibcLocales mp4v2 libid3tag libmad libopus libshout libsndfile
        libusb1 libvorbis opusfile pkgconfig portaudio portmidi protobuf qt5.full
        rubberband sqlite taglib upower
      ];
      sconsFlags = [
        "build=release"
        "optimize=native"
        "qtdir=${qt5.full}"
        "faad=1"
        "opus=1"
      ];
      fixupPhase = ''
        wrapProgram $out/bin/mixxx \
          --set LOCALE_ARCHIVE ${glibcLocales}/lib/locale/locale-archive;
      '';
      meta = with stdenv.lib; {
        homepage = https://mixxx.org;
        description = "Digital DJ mixing software";
        license = licenses.gpl2Plus;
        maintainers = [ maintainers.aszlig maintainers.goibhniu maintainers.bfortz ];
        platforms = platforms.linux;
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
    mercurialFull
    (python.withPackages (ps: with ps; [
      ipython
    ]))
    ruby
    gcc
    gnumake
    icu
    unstable.clang-tools
    cmake
    docker
    ycmd
    silver-searcher
    clang-tools
   	python36Packages.livereload
    gdb
    rustfmt
    wireshark

    # internet
    latest.firefox-bin
    chromium
    google-chrome
    pidgin
    pidgin-otr
    unstable.skype
    unstable.slack
    unstable.soulseekqt
    gnome3.polari
    unstable.youtube-dl
    tdesktop
    signal-desktop
    wire-desktop

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
    mpc_cli
    mpdris2
    calibre
    qjackctl
    jack2Full
    gnome3.cheese
    unstable.mixxx

    # editing
    gimp-with-plugins
    inkscape
    libreoffice-fresh
    xournal
    pdftk
    gcolor2
    blender
    imagemagickBig
    okular
    poppler_utils

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
    nixos-1803.taffybar
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
  programs.ibus = {
    enable = true;
  };
  programs.bash.enableCompletion = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;

  virtualisation.virtualbox.host.enable = true;
  virtualisation.docker.enable = true;

  networking.hosts = {
    "163.172.144.97" = ["orion1"];
    "163.172.181.40" = ["orion3"];
    # zen
    "127.0.0.1" = [
      #"twitter.com"
    ];
  };

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

  programs.wireshark.enable = true;
  programs.dconf.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.dbus.packages = [ pkgs.gnome3.gnome-keyring pkgs.gnome3.gcr ];
  services.gnome3 = {
      gnome-keyring.enable = true;
      seahorse.enable = true;
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
       "docker"
       "wireshark"
     ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
