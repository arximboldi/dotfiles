{ inputs, config, pkgs, ... }@arg:

let
  unstable = import inputs.nixos-unstable {
    system = pkgs.stdenv.hostPlatform.system;
    config = config.nixpkgs.config;
  };

  overlay = self: super: {
    my-emacs = self.emacs-gtk.override {
      withNativeCompilation = true;
      withTreeSitter = true;
    };

    my-emacs-with-packages = (self.emacsPackagesFor self.my-emacs).emacsWithPackages (ps: with ps; [
      treesit-grammars.with-all-grammars
    ]);

    gg = self.rustPlatform.buildRustPackage (attrs: {
      pname = "gg";
      version = "0.2.8";
      nativeBuildInputs = [ self.perl ];
      buildInputs = [ self.openssl ];
      src = self.fetchFromGitHub {
        owner = "frankwiles";
        repo = "gg";
        tag = "v${attrs.version}";
        hash = "sha256-M5MzGqEFXF8B5NCzjnCWtXH68LQpAFz7I2POaC+eAqA=";
      };
      cargoHash = "sha256-Hu7IzMOt9lWE9SvS9NhyJOyH39urDs7Fcx7ZY3gYFL0=";
    });
  };

in
{
  nixpkgs.overlays = [ overlay ];

  environment.systemPackages = with pkgs; [
    # editors
    zile
    emacs-pgtk
    # my-emacs-with-packages
    vscode
    zeal
    gedit

    # languages, compilers, build systems
    ruby
    gcc
    gnumake
    ninja
    icu
    clang
    llvm
    cmake
    # bazel
    nodejs

    # document generation
    hugo
    pandoc
    ispell
    texlive.combined.scheme-medium
    librsvg

    # profiling and debugging
    perf
    hotspot
    sysprof
    valgrind
    lldb
    gdb
    # kcachegrind

    # utils
    direnv
    xsel
    jq
    # clipgrab
    silver-searcher
    mmv
    magic-wormhole
    unzip
    unrar
    wget
    tree
    ghex

    # version control
    gitFull
    gh
    gg
    git-annex
    git-annex-remote-googledrive
    # git-annex-remote-rclone
    # git-annex-remote-dbx
    git-annex-metadata-gui
    rclone
    git-lfs
    mercurialFull

    # python
    ((python3.withPackages (ps: with ps; [
      ipython
      livereload
      # pafy
      pyliblo3
      twilio
      inquirer
      lxml
      tabulate
    ])).override (args: { ignoreCollisions = true; }))

    # code formatters and linters
    clang-tools
    rustfmt
    # nodePackages.standard
    nodePackages.prettier
    cmake-format
    nixfmt-rfc-style

    # language support in emacs
    tabnine
    bear
    ycmd

    # ai agents
    unstable.aider-chat
    unstable.plandex
    unstable.claude-code
    unstable.codex
    unstable.gemini-cli
    inputs.claudebox.packages.${pkgs.stdenv.hostPlatform.system}.default
    copilot-language-server
    github-copilot-cli
    gh-copilot

    # deployment
    google-cloud-sdk
    android-tools

    # networking
    wireshark
  ];

  programs.wireshark.enable = true;

  programs.adb.enable = true;

  # compiling remotely
  services.distccd = {
    enable = true;
    zeroconf = true;
    openFirewall = true;
    logLevel = "info";
    stats.enable = true;
    # bassically --allow-private
    # https://github.com/distcc/distcc/blob/66bf4c56f6af2243c48748139c078f4f01cd639b/src/dopt.c#L134C46-L141C57
    allowedClients = [
      "192.168.0.0/16"
      "10.0.0.0/8"
      "172.16.0.0/12"
      "127.0.0.0/8"
      "fe80::/10"
      "fc00::/7"
      "::1/128"
    ];
  };

  systemd.services.distccd.environment = {
    LISTENER = "::"; # for ipv6 support
    DISTCCD_PATH = builtins.concatStringsSep ":" ["${pkgs.gcc}/bin"];
  };
}
