{ config, pkgs, ... }:

let

  fetchFromGitHub = (import <nixpkgs> {}).fetchFromGitHub;
  musnix-src = fetchFromGitHub {
    owner  = "musnix";
    repo   = "musnix";
    rev    = "ec088e16dee5103b41bb422000e518a8f95926c4";
    sha256 = "196zpyk3ngaglx2hmpzg285r8lqhw74bbs5c6m77gg4mq6ai9fdn";
  };

in
{
  imports = [
    musnix-src.outPath
  ];
  musnix.enable = true;
}
