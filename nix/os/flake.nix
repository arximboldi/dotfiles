{
  description = "nixos configurations, with flakes";

  inputs = {
    nixos.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixos-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixos, ... }@inputs: {
    nixosConfigurations.ce3 = nixos.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ./ce3/configuration.nix
      ];
    };
  };
}
