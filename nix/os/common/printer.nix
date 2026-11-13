
{ config, pkgs, ... }:

{
  imports = [
    # This explicit import seems to be not needed anymore
    # <nixpkgs/nixos/modules/services/hardware/sane_extra_backends/brscan4.nix>
  ];

  environment.systemPackages = with pkgs; [
    brgenml1lpr
  ];

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.brgenml1cupswrapper ];

  # The home Brother, over the network (found via avahi), as the default
  # destination: lpr (and so emacs' print-buffer) needs one.
  hardware.printers = {
    ensurePrinters = [{
      name = "DCP-L2520DW";
      location = "home";
      deviceUri = "dnssd://Brother%20DCP-L2520DW%20series._ipp._tcp.local/?uuid=e3248000-80ce-11db-8000-a86bad6d488b";
      model = "brother-BrGenML1-cups-en.ppd";
      ppdOptions.PageSize = "A4";
    }];
    ensureDefaultPrinter = "DCP-L2520DW";
  };

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
