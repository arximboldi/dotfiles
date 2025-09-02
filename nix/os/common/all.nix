{ inputs, config, pkgs, ... }@arg:
{
  imports = [
    ./nix.nix
    ./printer.nix
    ./sound.nix
    ./desktop.nix
    ./users.nix
    ./gaming.nix
    ./syncthing.nix
    ./mpd.nix
    ./devel.nix
    ./internet.nix
    ./virtualization.nix
    ./networking.nix
  ];
}
