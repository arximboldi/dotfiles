
{ config, pkgs, inputs, ... }:

let
  unstable = import inputs.nixos-unstable {
    system = pkgs.stdenv.hostPlatform.system;
    config = config.nixpkgs.config;
  };
in
{
 # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    iterm2
    slack
    gdb
    # valgrind
    lldb
    llvm
    clang-tools
    vscode
    vim
    ((emacsPackagesFor emacs).emacsWithPackages (
      epkgs: [ epkgs.treesit-grammars.with-all-grammars ]
    ))
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
    unstable.devenv
    htop
    _1password-cli
    _1password-gui
    obsidian
    unstable.claude-code
    unstable.codex
    nodejs
    nixfmt-rfc-style
    nodePackages.typescript-language-server
    zen-browser
    #telegram-desktop
    #whatsapp-for-mac
    tart
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
    #jetbrains-mono
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

  nixpkgs.overlays = [
    (self: super: {
      zen-browser = inputs.zen-browser.packages."${self.stdenv.hostPlatform.system}".default;
    })
  ];

  services.tailscale.enable = true;

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;
  # nix.configureBuildUsers = true;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  # system.stateVersion = 4;

  # You should generally set this to the total number of logical cores in your system.
  # $ sysctl -n hw.ncpu
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.max-jobs = 1;
  nix.settings.build-cores = 1;
  nix.settings.trusted-users = ["raskolnikov"];

  system.defaults.NSGlobalDomain = {
    "com.apple.swipescrolldirection" = false;
  };

  system.primaryUser = "raskolnikov";

  # system.keyboard.swapLeftCtrlAndFn = true;
  # system.keyboard.enableKeyMapping = true;
  # Re-apply the swap on every login, since hidutil mappings don't persist across reboots
  launchd.daemons.swap-ctrl-fn-v2 = {
  serviceConfig = {
    Label = "org.local.swap-ctrl-fn-v2";
    ProgramArguments = [
      "/bin/sh"
      "-c"
      ''
        for i in $(seq 1 20); do
          /usr/bin/hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x7000000E0,"HIDKeyboardModifierMappingDst":0xFF00000003},{"HIDKeyboardModifierMappingSrc":0xFF00000003,"HIDKeyboardModifierMappingDst":0x7000000E0}]}' > /dev/null
          sleep 1
        done
      ''
    ];
    RunAtLoad = true;
  };
  };

  # make bash the default shell for the user raskolnikov
  users.users.raskolnikov.shell = pkgs.bashInteractive;
  system.activationScripts.postActivation.text =
    let username = "raskolnikov"; in ''
    TARGET_SHELL="/run/current-system/sw/bin/bash"
    CURRENT_SHELL=$(dscl . -read /Users/${username} UserShell | awk '{print $2}')
    if [ "$CURRENT_SHELL" != "$TARGET_SHELL" ]; then
      dscl . -create /Users/${username} UserShell "$TARGET_SHELL"
    fi
  '';

  # Make Ctrl+Arrow keys jump words in all Cocoa text editors
  system.activationScripts.keyBindings.text = ''
    KEYBINDINGS_DIR="/Users/raskolnikov/Library/KeyBindings"
    KEYBINDINGS_FILE="$KEYBINDINGS_DIR/DefaultKeyBinding.dict"
    mkdir -p "$KEYBINDINGS_DIR"
    cat > "$KEYBINDINGS_FILE" << 'EOF'
{
  /* Ctrl+Left/Right: jump words */
  "^\UF702" = "moveWordLeft:";
  "^\UF703" = "moveWordRight:";
  /* Ctrl+Shift+Left/Right: select word */
  "^$\UF702" = "moveWordLeftAndModifySelection:";
  "^$\UF703" = "moveWordRightAndModifySelection:";
}
EOF
  '';

  # disable Ctrl+Space and Ctrl+Option+Space input source switching, which conflicts with Emacs's default keybindings
  # this is wrong (wrong domain)
  # system.defaults.CustomUserPreferences = {
  #  NSGlobalDomain = { AppleSymbolicHotKeys = { "60" = { enabled = false; }; }; };
  # };

  # claude version
  system.defaults.CustomUserPreferences = {
    "com.apple.symbolichotkeys" = {
      AppleSymbolicHotKeys = {
        # Disable "Select the previous input source" (default: Ctrl+Space)
        "60" = {
          enabled = false;
        };
        # Disable "Select next source in Input menu" (default: Ctrl+Option+Space)
        "61" = {
          enabled = false;
        };
      };
    };
  };

  homebrew.casks = [ "telegram" "whatsapp" "syncthing-app" "linear" "utm" ];
  homebrew.enable = true;

}
