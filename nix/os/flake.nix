{
  description = "nixos configurations, with flakes";

  inputs = {
    nixos.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
    nix-darwin.inputs.nixpkgs.follows = "nixos";

    ucodenix.url = "github:e-tho/ucodenix";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    musnix.url = "github:musnix/musnix";
    musnix.inputs.nixpkgs.follows = "nixos";

    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    zen-browser.inputs.nixpkgs.follows = "nixos";

    dms.url = "github:AvengeMedia/DankMaterialShell";
    dms.inputs.nixpkgs.follows = "nixos";

    cantata.url = "github:nullobsi/cantata";
    cantata.inputs.nixpkgs.follows = "nixos";

    mpd-sima-gui.url = "github:arximboldi/mpd-sima-gui";
    mpd-sima-gui.inputs.nixpkgs.follows = "nixos";

    claudebox.url = "github:numtide/claudebox";
  };

  outputs = { self, nixos, nix-darwin, ... }@inputs: {
    darwinConfigurations = {
      # macbook pro (last intel gen)
      tyrell1 = nix-darwin.lib.darwinSystem {
        specialArgs = { inherit inputs; };
        modules = [
          ./tyrell0/darwin-configuration.nix
          {
            nixpkgs.hostPlatform = "x86_64-darwin";
          }
        ];

      };
    };

    nixosConfigurations = {
      # framework laptop with amd ai 300
      ce3 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./ce3/configuration.nix
        ];
      };

      # nxt desktop
      ce2 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./ce2/configuration.nix
        ];
      };

      # lenovo x1 carbon 5th gen
      ce1 = nixos.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./ce1/configuration.nix
        ];
      };
    };
  };
}
