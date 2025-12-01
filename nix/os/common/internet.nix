{ inputs, config, pkgs, ... }@arg:

let
  unstable = import inputs.nixos-unstable {
    system = pkgs.stdenv.hostPlatform.system;
    config = config.nixpkgs.config;
  };

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
  ) {
    system = pkgs.stdenv.hostPlatform.system;
  };

  overlay = self: super: {
    zen-browser = inputs.zen-browser.packages."${self.stdenv.hostPlatform.system}".default;
  };

in
{
  nixpkgs.overlays = [ overlay ];

  # chromecast support
  networking.firewall.allowedTCPPorts = [ 8010 ];

  environment.systemPackages = with pkgs; [
    # internet
    # flashplayer
    wirelesstools
    iw

    # browsers
    tor-browser
    firefox
    chromium
    google-chrome
    zen-browser

    # communications
    (pidgin.override {
      plugins = [
        pidginPackages.pidgin-otr
        # purple-facebook
        # purple-hangouts
        # purple-matrix
        # telegram-purple
      ];
    })
    # station
    # franz
    # rambox
    # unstable.teams
    # skypeforlinux
    unstable.slack
    unstable.teams-for-linux
    unstable.discord
    unstable.webcord
    unstable.telegram-desktop
    unstable.signal-desktop
    zapzap
    signal-cli
    zoom-us
    polari
    signal-desktop
    # wire-desktop

    # mail
    notmuch
    isync
    afew
    notify-desktop
    gnupg
    msmtp
    evolution
    thunderbird

    # p2p
    transmission_4-gtk
    amule
    # soulseekqt
    nicotine-plus
    yt-dlp

    # for emule
    nixos-wine6.wine64Packages.stableFull
    nixos-wine6.winetricks
  ];
}
