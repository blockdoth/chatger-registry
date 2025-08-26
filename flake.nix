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
      perSystem ={ pkgs, ... }:
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
              hlint
            ];
          };
          packages.default = pkgs.stdenv.mkDerivation {
            pname = "regger";
            version = "0.0.1";
            src = ./.;

            buildInputs = [
              (pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
                network
                split
                sqlite-simple
                cryptonite
              ]))       
            ];

            buildPhase = ''
              ghc -threaded -O2 -o regger main.hs
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp index.html $out/bin
              cp regger $out/bin
            '';
          };
      };        
    };
}
