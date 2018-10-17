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

    # browsers
    firefox
    chromium

    # utils
    stow
    wget
    usbutils
    trash-cli
    psmisc
    glxinfo
    htop

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

    # mail
    notmuch
    isync
    afew
    notify-desktop
    gnupg
    unstable.brgenml1lpr
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
    ];
  };

  programs.bash.enableCompletion = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;


  services.printing.enable = true;
  services.printing.drivers = [ unstable.brgenml1cupswrapper ];


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
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";
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
     ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
