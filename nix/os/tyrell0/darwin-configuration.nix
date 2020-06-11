
{ config, pkgs, ... }:

{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # iterm2
    # slack
    gdb
    lldb
    valgrind
    vim
    emacs
    stow
    bash
    coreutils
    trash-cli
    gitFull
    clang-tools
    ((python.withPackages (ps: with ps; [
      ipython
      livereload
    ])).override (args: { ignoreCollisions = true; }))
    # cmake
    # (python3.withPackages (ps: [ps.setuptools ps.pip]))
  ];

  # Other software
  #   - homebrew
  #   - conan
  #
  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    corefonts
    inconsolata
    ubuntu_font_family
    dejavu_fonts
    fira
    fira-mono
    fira-code
    fira-code-symbols
    source-sans-pro
    # emojione
    roboto
    roboto-mono
    source-code-pro
    iosevka
    iosevka-bin
  ];

  nixpkgs.config.allowUnfree = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  nix.package = pkgs.nix;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.maxJobs = 1;
  nix.buildCores = 1;
}
