{ inputs, config, pkgs, ... }@arg:
let
  unstable = import inputs.nixos-unstable {
    system = pkgs.stdenv.hostPlatform.system;
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
  nix.nixPath = [
    "nixpkgs=${inputs.nixos.outPath}"
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "olm-3.2.16"
    "openssl-1.1.1u"
    "python-2.7.18.6"
    "python2.7-pyjwt-1.7.1"
    "libdwarf-20181024"
    "python2.7-certifi-2021.10.8"
    "electron-36.9.5"
    # "qtwebengine-5.15.19"
    "libsoup-2.74.3"
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
