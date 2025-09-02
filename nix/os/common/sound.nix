{ inputs, config, pkgs, ... }:

let
  musnix-src = builtins.fetchGit {
    url  = "https://github.com/musnix/musnix.git";
    rev  = "86ef22cbdd7551ef325bce88143be9f37da64c26";
  };

in
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
