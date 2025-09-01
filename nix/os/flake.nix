{
  description = "nixos configurations, with flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = { self, nixpkgs, ... }@inputs: {
    nixosConfigurations.ce3 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./ce3/configuration.nix
      ];
    };
  };
}
