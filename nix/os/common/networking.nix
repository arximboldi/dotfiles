{ inputs, config, pkgs, ... }@arg:
{
  # Enable networking
  networking.networkmanager.enable = true;

  networking.firewall.enable = false;

  networking.hosts = {
    "163.172.144.97" = ["orion1"];
    "163.172.181.40" = ["orion3"];
    "162.55.168.220" = ["orion4"];
    "78.46.255.228" = ["wendy"];
    "162.55.172.27" = ["laurie"];
    "49.12.219.169" = ["daphne"];
    "162.55.48.161" = ["suzanne"];
    "167.235.96.165" = ["laurel"];
    "142.132.141.13" = ["clara"];
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    # nssmdns6 = true;
    openFirewall = true;
    publish.enable = true;
    publish.userServices = true;
  };

  environment.systemPackages = with pkgs; [
    # lte internet
    modemmanager
    mobile-broadband-provider-info
    usb-modeswitch
    usb-modeswitch-data


    # tailscale
  ];
}
