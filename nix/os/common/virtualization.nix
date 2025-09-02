{ inputs, config, pkgs, ... }:

{
  # enable virt-manager
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = ["your_username"];
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;

  environment.systemPackages = with pkgs; [
    docker
    gnome-boxes
  ];
}
