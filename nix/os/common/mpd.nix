{ inputs, config, pkgs, ... }@arg:
let
  unstable = import inputs.nixos-unstable {
    system = pkgs.system;
    config = config.nixpkgs.config;
  };

  overlay = self: super: {
    # latest version for workaround
    mpd = unstable.mpd;

    covergrid = with super;  python3Packages.buildPythonApplication rec {
      pname = "covergrid";
      version = "2.1.12";
      src = fetchGit {
        url = "https://gitlab.com/coderkun/mcg.git";
        rev = "17fe4ee8cad2265e0283f33be40508561687cddb";
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

    # beets plugin
    beetcamp = (self.python3Packages.buildPythonApplication {
      pname = "beets-beetcamp";
      version = "0.21.0";
      src = self.fetchFromGitHub {
        repo = "beetcamp";
        owner = "snejus";
        rev = "64c7afc9d87682fb2b7c9f2deb76525e44afb248";
        sha256 = "sha256-d0yvOyfxPPBUpoO6HCWfMq2vVw+CcQo16hx+JRDMkBw=";
      };
      format = "pyproject";
      buildInputs = with self.python3Packages; [ poetry-core ];
      propagatedBuildInputs = with self.python3Packages; [
        setuptools requests cached-property pycountry dateutil ordered-set
      ];
      checkInputs = with self.python3Packages; [
        # pytestCheckHook
        pytest-cov
        pytest-randomly
        pytest-lazy-fixture
        rich
        tox
        types-setuptools
        types-requests
      ] ++ [
        self.beets
      ];
      meta = {
        homepage = "https://github.com/snejus/beetcamp";
        description = "Bandcamp autotagger plugin for beets.";
        license = self.lib.licenses.gpl2;
        inherit (self.beets.meta) platforms;
        maintainers = with self.lib.maintainers; [ rrix ];
      };
    });

    # custom version of beets with bandcamp plugin enabled
    my-beets = (self.beets.override {
      pluginOverrides = {
        bandcamp = {
          enable = true;
          propagatedBuildInputs = [ self.beetcamp ];
        };
      };
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

    # clients
    unstable.cantata # qt 6
    amberol
    gapless
    tauon
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
    mpc_cli

    # integrations
    mpdris2
    mpdas

    # library management
    my-beets
    tmsu
  ];
}
