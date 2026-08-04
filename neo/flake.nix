{
  description = "NeoCLI - The NeoHaskell CLI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "neo";
          version = "0.1.0";
          src = ./.;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };
          
          doCheck = false;
          
          # Use default stdenv inputs, rustPlatform handles most Apple SDK automatically
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            rustc
            rustfmt
            clippy
            # Frontend toolchain for the `assets/ide/` Vite project (the IDE
            # served by `neo ide`). The release `nix build` consumes the
            # already-built `assets/ide/dist/` tree from the source — Node is
            # only needed in the dev shell so contributors can run
            # `npm install && npm run build` after editing the IDE source.
            nodejs_22
            pnpm
          ];
        };
      }
    );
}
