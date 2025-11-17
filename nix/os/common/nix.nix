{ inputs, config, pkgs, ... }@arg:
let
  unstable = import inputs.nixos-unstable {
    system = pkgs.system;
    config = config.nixpkgs.config;
  };

in
{
  nixpkgs.config.allowUnfree = true;

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "olm-3.2.16"
    "openssl-1.1.1u"
    "python-2.7.18.6"
    "python2.7-pyjwt-1.7.1"
    "libdwarf-20181024"
    "python2.7-certifi-2021.10.8"
  ];

  environment.systemPackages = with pkgs; [
    nix-index
    cached-nix-shell
    unstable.cachix
    niv
  ];

  # make bazel be happy
  # services.envfs.enable = true;
  # system.activationScripts.binbash = {
  #   deps = [ "binsh" ];
  #   text = ''
  #        ln -s /bin/sh /bin/bash
  #   '';
  # };
}
