{ inputs, config, pkgs, ... }@arg:

let
  unstable = import inputs.nixos-unstable {
    system = pkgs.system;
    config.allowUnfree = true;
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
    system = pkgs.system;
  };

  overlay = self: super: {
    telegram-alias = pkg: self.runCommand "telegram-alias" {} ''
      mkdir -p $out/bin
      ln -s ${pkg}/bin/telegram-desktop $out/bin/telegram
    '';

    zen-browser = inputs.zen-browser.packages."${self.system}".default;
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
        pidgin-otr
        purple-facebook
        purple-hangouts
        purple-matrix
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
    (telegram-alias unstable.telegram-desktop)
    wasistlos
    zapzap
    signal-desktop
    signal-cli
    zoom-us
    polari
    signal-desktop
    # wire-desktop

    # mail
    unstable.notmuch
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
    soulseekqt
    nicotine-plus
    yt-dlp

    # for emule
    nixos-wine6.wine64Packages.stableFull
    nixos-wine6.winetricks
  ];
}
