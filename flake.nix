{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      perSystem =
        {
          pkgs,
          ...
        }:
        let

        in
        {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              (pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
                network
                split
                sqlite-simple
                cryptonite
              ]))              
              ghc
              haskell-language-server
              cabal-install
              hlint
              python3
            ];
          };
        };
    };
}
