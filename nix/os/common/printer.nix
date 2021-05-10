
{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/services/hardware/sane_extra_backends/brscan4.nix>
  ];

  environment.systemPackages = with pkgs; [
    brgenml1lpr
  ];

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.brgenml1cupswrapper ];

  hardware.sane = {
    enable = true;
    brscan4 = {
      enable = true;
      netDevices = {
        home = {
          model = "DCP-L2520DW";
          ip = "192.168.178.2";
        };
      };
    };
  };
}
