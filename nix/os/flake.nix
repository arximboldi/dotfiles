{
  description = "nixos configurations, with flakes";

  inputs = {
    nixos.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware";
    musnix.url = "github:musnix/musnix";

    zen-browser.url = "github:0xc000022070/zen-browser-flake";
  };

  outputs = { self, nixos, ... }@inputs: {
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
