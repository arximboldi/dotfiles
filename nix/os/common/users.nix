{ config, pkgs, ... }:

{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.raskolnikov = {
     isNormalUser = true;
     uid = 1000;
     extraGroups = [
       "wheel"
       "audio"
       "users"
       "scanner"
       "lp"
       "networkmanager"
       "docker"
       "wireshark"
     ];
  };
  users.extraUsers.nikki = {
     isNormalUser = true;
     uid = 1001;
     extraGroups = [
       "wheel"
       "audio"
       "users"
       "scanner"
       "lp"
       "networkmanager"
     ];
  };
  nix.trustedUsers = [ "raskolnikov" "nikki" ];
}
