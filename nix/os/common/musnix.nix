{ config, pkgs, ... }:

let

  fetchFromGitHub = (import <nixpkgs> {}).fetchFromGitHub;
  musnix-src = fetchFromGitHub {
    owner  = "musnix";
    repo   = "musnix";
    rev    = "86ef22cbdd7551ef325bce88143be9f37da64c26";
    sha256 = "sha256-lvee0rhKTpJEDl1SC4F3Kvz88snOU8OMTJBsPH1pVBo=";
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
