{ inputs, config, pkgs, ... }:
{
  # syncthing
  services.syncthing = {
    enable = true;
    user = "raskolnikov";
    dataDir = "/home/raskolnikov/sync";    # Default folder for new synced folders
    configDir = "/home/raskolnikov/.config/syncthing";
  };

  environment.systemPackages = with pkgs; [
    syncthing
    # qsyncthingtray
  ];
}
