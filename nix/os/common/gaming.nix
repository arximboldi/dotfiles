{ inputs, config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    protontricks
    steam
    steam-run
    sidequest
    scrcpy
    gamescope

    # shooters
    ioquake3
    quake3pointrelease
    quake3e
    openarena
    superTuxKart
    gnujump
    alephone-marathon
    alephone-durandal
    alephone-infinity
    alephone-red

    # alienarena
    # liquidwar
    # liquidwar5
    # unvanquished

    # emulators
    retroarch
    mame
    dosbox
    qemu
    qtemu

    # gaming
    # moonlight-embedded
    moonlight-qt # remote gaming
    # winePackages.stableFull
    # wine64Packages.stagingFull
    # wineWow64Packages.waylandFull
    # winetricks
  ];
}
