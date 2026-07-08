{
  # macOS system defaults managed by nixmac.

  system.defaults.dock = {
    orientation = "left";
  };

  system.defaults.NSGlobalDomain = {
    NSShowAllExtensions = true;
  };

  system.defaults.controlcenter.Sound = true;
}
