# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ inputs, config, pkgs, ... }@arg:

let
  unstable = inputs.nixpkgs-unstable;

in
{
  imports = [
    ./hardware-configuration.nix
    ../common/all.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ce3";
  time.timeZone = "Europe/Berlin"; #"America/New_York"; #"America/Los_Angeles"; # "Europe/Moscow"; # "America/Vancouver"; # "America/New_York"; # "Europe/Berlin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # i18n.extraLocaleSettings = {
  #   LC_ADDRESS = "de_DE.UTF-8";
  #   LC_IDENTIFICATION = "de_DE.UTF-8";
  #   LC_MEASUREMENT = "de_DE.UTF-8";
  #   LC_MONETARY = "de_DE.UTF-8";
  #   LC_NAME = "de_DE.UTF-8";
  #   LC_NUMERIC = "de_DE.UTF-8";
  #   LC_PAPER = "de_DE.UTF-8";
  #   LC_TELEPHONE = "de_DE.UTF-8";
  #   LC_TIME = "de_DE.UTF-8";
  # };

  # Damn Rubygems and Bitbucket...

  services.fprintd.enable = true;

  system.stateVersion = "25.05"; # Did you read the comment?
}
