{ config, pkgs, ... }:

let
  musnix-src = builtins.fetchGit {
    url  = "https://github.com/musnix/musnix.git";
    rev  = "86ef22cbdd7551ef325bce88143be9f37da64c26";
  };

in
{
  imports = [
    musnix-src.outPath
  ];
  musnix.enable = true;
  #musnix.kernel.realtime = true;
  #musnix.kernel.packages = pkgs.linuxPackages_latest_rt;
}
