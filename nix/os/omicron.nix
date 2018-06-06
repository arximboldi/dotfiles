{ config, pkgs, ... }:

let
  musnixSrc = (import <nixpkgs> {}).fetchFromGitHub {
    owner  = "musnix";
    repo   = "musnix";
    rev    = "cec9d0529977e2db2a273f33c3261620098465ed";
    sha256 = "1ybja7i5c8nh0drlp4pjxkp3v6zp7f8hi8d8nwbsgf2ym9cxjlwf";
};

in
{
  imports = [ musnixSrc.outPath ];
  musnix.enable = true;

  boot.loader.grub.device = "/dev/sdb";
  networking.hostName = "nixos";

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
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTR{idVendor}=="156d", ATTR{idProduct}=="4007", GROUP="wheel"
  '';
  environment.systemPackages = with pkgs; [
    zile
    emacs
    firefox
    gitAndTools.gitFull
    stow
    wget
    libnotify
    numix-gtk-theme
    numix-cursor-theme
    numix-icon-theme
    numix-icon-theme-circle
    taffybar
    dmenu
    ibus
    xdotool-arximboldi
    pa_applet
    usbutils
    trash-cli
    psmisc
    glxinfo
    qtcreator
    qt59.full
    gdb
    cmake
    gcc
    pavucontrol
    chromium
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
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.systemWide = true;
  hardware.pulseaudio.daemon.config = {
    default-fragments = 2;
    default-fragment-size-msec = 125;
    realtime-scheduling = "yes";
    realtime-priority = 5;
  };

  services.xserver.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.xmonad.extraPackages = hs: [hs.taffybar];
  users.extraUsers.raskolnikov = {
     isNormalUser = true;
     uid = 1000;
     extraGroups = [
       "wheel"
       "audio"
       "users"
     ];
  };

  system.stateVersion = "18.03";
}
