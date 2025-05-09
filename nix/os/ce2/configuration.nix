# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../common/printer.nix
      ../common/musnix.nix
      ../common/desktop.nix
      ../common/users.nix
      ../common/ssh-phone-home.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ce2";
  time.timeZone = "Europe/Berlin";

  nixpkgs.config.allowUnfree = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  # https://nixos.wiki/wiki/Nvidia

  virtualisation.docker.enable = true;
  virtualisation.podman.enable = true;
  #virtualisation.docker.enableOnBoot = true;
  #virtualisation.docker.enableNvidia = true;
  #virtualisation.podman.enableOnBoot = true;
  #virtualisation.podman.enableNvidia = true;
  # systemd.enableUnifiedCgroupHierarchy = false;
  hardware.nvidia-container-toolkit.enable = true;
  hardware.nvidia-container-toolkit.mount-nvidia-executables = false;

  #environment.sessionVariables = {
  #  DOCKER_HOST="unix:///var/run/docker.sock";
  #};
  #virtualisation.docker.rootless = {
  #  enable = true;
  #  setSocketVariable = true;
  #  daemon.settings = {
  #    default-runtime = "nvidia";
  #    runtimes.nvidia.path = "${pkgs.nvidia-docker}/bin/nvidia-container-runtime";
  #  };
  #};

  nixpkgs.config.nvidia.acceptLicense = true;
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;

    # Enable the Nvidia settings menu,
	  # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver
    # version for your specific GPU.
    # package = config.boot.kernelPackages.nvidiaPackages.vulkan_beta;
    # package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  };

  services.ssh-phone-home = {
    enable = true;
    localUser = "raskolnikov";
    remoteHostname = "sinusoid.es";
    remotePort = 5488;
    remoteUser = "raskolnikov";
    bindPort = 5489;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
