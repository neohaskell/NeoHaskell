{ lib, stdenvNoCC, fetchurl }:
let
  version = "0.2.97";
  releases = {
    aarch64-darwin = {
      platform = "Darwin_arm64";
      hash = "sha256-bbse80p+NLse4NsYjRaC6jW4LZ2QEmJkl/bGWeVafLU=";
    };
    x86_64-darwin = {
      platform = "Darwin_x86_64";
      hash = "sha256-y6nPBx9pxHsv72CkxzUAK2y9QWUzkgQG9y6sDPf/7Do=";
    };
    aarch64-linux = {
      platform = "Linux_arm64";
      hash = "sha256-RNSOPUpkCQU/3sb8ZRgyJGfXmi1b6PcsAhsEIeKsET0=";
    };
    x86_64-linux = {
      platform = "Linux_x86_64";
      hash = "sha256-Z7SXalbzPqeHC3mJEuc47ETu8451eK57Uy8t7xxzpa0=";
    };
  };
  release = releases.${stdenvNoCC.hostPlatform.system};
in stdenvNoCC.mkDerivation {
  pname = "act";
  inherit version;
  src = fetchurl {
    url =
      "https://github.com/nektos/act/releases/download/v${version}/act_${release.platform}.tar.gz";
    inherit (release) hash;
  };
  sourceRoot = ".";
  dontStrip = true;
  installPhase = ''
    runHook preInstall
    install -Dm755 act "$out/bin/act"
    install -Dm644 LICENSE "$out/share/licenses/act/LICENSE"
    runHook postInstall
  '';
  meta = {
    description = "Run GitHub Actions locally";
    homepage = "https://github.com/nektos/act";
    license = lib.licenses.mit;
    mainProgram = "act";
    platforms = builtins.attrNames releases;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
