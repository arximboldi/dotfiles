{ config, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.raskolnikov = {
     isNormalUser = true;
     uid = 1000;
     extraGroups = [
       "wheel"
       "audio"
       "pipewire"
       "users"
       "scanner"
       "lp"
       "networkmanager"
       "docker"
       "wireshark"
       "sysprof"
       "adbusers"
       "plugdev"
     ];
  };

  nix.settings.trusted-users = [ "raskolnikov" ];
}
