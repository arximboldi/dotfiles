# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }@arg:

let
  unstable = import <nixos-unstable> {};

in
{
  imports = [
    ./hardware-configuration.nix
    ../common/printer.nix
    ../common/musnix.nix
    ../common/desktop.nix
    ../common/users.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ce1";
  time.timeZone = "Europe/Berlin"; # "Europe/Moscow"; # "America/Vancouver"; # "America/New_York"; # "Europe/Berlin";

  # Damn Rubygems and Bitbucket...
  networking.enableIPv6 = false;
  nixpkgs.config.allowUnfree = true;

  systemd.services.modem-manager.enable = true;
  # https://nixos.wiki/wiki/Accelerated_Video_Playback
  hardware.opengl.extraPackages = with pkgs; [
    libva intel-media-driver vaapiIntel libvdpau-va-gl vaapiVdpau # intel-ocl
  ];
  # hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva vaapiIntel libvdpau-va-gl vaapiVdpau ];

  hardware.acpilight.enable = true;

  services.xserver = {
    videoDrivers = [ "modesetting" ];
    #useGlamor = true;
    #videoDrivers = [ "intel" ];
    # deviceSection = ''
    #   Option "DRI" "2"
    #   Option "TearFree" "true"
    # '';
  };

  services.fprintd.enable = true;

  #services.mediatomb.enable = true;
  #services.mediatomb.openFirewall = true;
  #services.mediatomb.mediaDirectories = [
  #  {
  #    path = "/run/media/raskolnikov/elemento/videos/pelis/erotica/vr";
  #    recursive = true;
  #  }
  #];
  services.minidlna.enable = true;
  services.minidlna.mediaDirs = [
    "/run/media/raskolnikov/elemento/videos/pelis/erotica/vr"
    "/home/raskolnikov/media/videos"
  ];
  systemd.services.minidlna.serviceConfig.User = pkgs.lib.mkForce "root";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?
}
