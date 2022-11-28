{ config, pkgs, ... }:

let

  fetchFromGitHub = (import <nixpkgs> {}).fetchFromGitHub;
  musnix-src = fetchFromGitHub {
    owner  = "musnix";
    repo   = "musnix";
    rev    = "52e344b30a364c709eb314e4f139074a7f81b19b";
    sha256 = "sha256-crgQuuIXAJvWtheOuyACOeRtym4AERR2SsnE01e0VLc=";
  };

in
{
  imports = [
    musnix-src.outPath
  ];
  musnix.enable = true;
  musnix.kernel.realtime = true;
  musnix.kernel.packages = pkgs.linuxPackages_latest_rt;
}
