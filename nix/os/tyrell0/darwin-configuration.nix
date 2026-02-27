
{ config, pkgs, ... }:

{
 # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # iterm2
    # slack
    gdb
    # valgrind
    lldb
    llvm
    clang-tools
    vscode
    vim
    emacs30
    stow
    # bash
    coreutils
    trash-cli
    gitFull
    gh
    cmake
    (python3.withPackages (ps: with ps; [
      ipython
      livereload
    ]))
    copilot-language-server
    devenv
    htop
    _1password-cli
    _1password-gui
    claude-code
    nodejs
    nixfmt-rfc-style
  ];

  fonts.packages = with pkgs; [
    aileron
    cantarell-fonts
    corefonts
    dejavu_fonts
    fira
    fira-code
    fira-code-symbols
    fira-mono
    helvetica-neue-lt-std
    inconsolata
    iosevka
    iosevka-bin
    jetbrains-mono
    nerd-fonts.iosevka
    nerd-fonts.jetbrains-mono
    nerd-fonts.victor-mono
    nerd-fonts.zed-mono
    noto-fonts
    noto-fonts-color-emoji
    openmoji-color
    roboto
    roboto-mono
    source-code-pro
    source-sans-pro
    twemoji-color-font
    ubuntu-classic
  ];

  nixpkgs.config.allowUnfree = true;

  services.tailscale.enable = true;

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;
  # nix.configureBuildUsers = true;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.max-jobs = 1;
  nix.settings.build-cores = 1;
  nix.settings.trusted-users = ["raskolnikov"];
}
