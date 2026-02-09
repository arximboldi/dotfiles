{ inputs, config, pkgs, ... }@arg:
let
  unstable = import inputs.nixos-unstable {
    system = pkgs.stdenv.hostPlatform.system;
    config = config.nixpkgs.config;
  };

  overlay = self: super: {
    # latest version for workaround

    covergrid = with super;  python3Packages.buildPythonApplication rec {
      pname = "covergrid";
      version = "3.2.1";
      src = fetchGit {
        url = "https://gitlab.com/coderkun/mcg.git";
        rev = "75b99e5820f0f28f5cfb52507c9fb20f15b454fb";
      };
      postInstall = ''
         cp -r data $out/lib/python3.8/site-packages/mcg/
         glib-compile-schemas $out/share/glib-2.0/schemas
      '';
      nativeBuildInputs = [ glib wrapGAppsHook ];
      buildInputs = [ glib gtk3 gobject-introspection ];
      propagatedBuildInputs = with python3Packages; [
        glib gtk3
        pygobject3
        keyring
        avahi
      ];
      strictDeps = false;
    };

    cantata = inputs.cantata.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (attrs: rec {
      buildInputs = attrs.buildInputs ++ [ super.kdePackages.karchive ];
    });

    cantata-latest = super.cantata.overrideAttrs (attrs: rec {
      version = "3.4.0";
      patches = [];
      src = super.fetchFromGitHub {
        owner = "nullobsi";
        repo = "cantata";
        rev = "v3.4.0";
        sha256 = "sha256-jwIsuNgsd1TFb1Zkyen/AulGQfVY2RWKfAJaWvg4WMI=";
      };
      cmakeFlags = attrs.cmakeFlags ++ [
        "-DBUNDLED_KCATEGORIZEDVIEW=on"
        "-DBUNDLED_KARCHIVE=on"
        "-DBUNDLED_FONTAWESOME=on"
      ];
    });
  };

in
{
  nixpkgs.overlays = [ overlay ];

  # https://nixos.wiki/wikui/MPD
  services.mpd = {
    enable = true;
    user = "raskolnikov";
    musicDirectory = "/home/raskolnikov/media/music/mpd";
    dataDir = "/home/raskolnikov/.config/mpd";
    network.listenAddress = "any";
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "PipeWire Output"
      }
    '';
  };
  systemd.services.mpd.environment = {
    # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
    XDG_RUNTIME_DIR = "/run/user/${toString config.users.users.raskolnikov.uid}";
  };

  environment.systemPackages = with pkgs; [
    mpd

    mpd-sima

    # clients
    cantata # qt 6

    amberol
    gapless
    tauon
    euphonica
    # gmpc
    plattenalbum
    ymuse
    # covergrid
    ncmpc
    ncmpcpp
    ario
    sonata
    # clerk
    mmtc
    mpc

    # integrations
    mpdris2-rs
    mpdas

    # library management
    beets
    tmsu
  ];
}
