{ pkgs ? import ../nix/nixpkgs.nix { } }:
pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = { };
  modifier = drv: pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontHaddock drv);
  overrides = self: super: rec {
    mkDerivation = args: super.mkDerivation (args // {
      doCheck = false;
      doHaddock = false;
      enableLibraryProfiling = false;
      enableExecutableProfiling = false;
      jailbreak = true;
    })
  };
}
