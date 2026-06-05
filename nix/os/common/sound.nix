{ inputs, config, pkgs, ... }:
{
  imports = [
    inputs.musnix.nixosModules.musnix
  ];

  musnix.enable = true;
  # musnix.kernel.realtime = true;
  # musnix.kernel.packages = pkgs.linuxPackages_latest_rt;

  security.rtkit.enable = true;

  # sound.enable = true;

  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
    alsa.support32Bit = false;
    jack.enable = false;
  };

  services.pulseaudio = {
    enable = false;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
  };
}
