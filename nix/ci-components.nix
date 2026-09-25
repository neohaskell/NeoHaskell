{ pkgs, flake }:
let
  # These are the generated attributes of our pinned hix project. Keep the
  # components themselves as derivations: this bundle only references them.
  names = [
    "nhcore:lib:nhcore"
    "nhintegrations:lib:nhintegrations"
    "nhtestbed:lib:nhtestbed"
    "nhcore:test:nhcore-test-core"
    "nhcore:test:nhcore-test-auth"
    "nhcore:test:nhcore-test-integration"
    "nhcore:test:nhcore-test-service"
    "nhintegrations:test:nhintegrations-test"
    "nhtestbed:exe:nhtestbed"
  ];
  components = builtins.listToAttrs (map (name: {
    inherit name;
    value = flake.packages.${name};
  }) names);
  runtime = pkgs.buildEnv {
    name = "neohaskell-ci-runtime";
    paths = with pkgs; [ bash coreutils curl hurl postgresql poppler_utils python3 ];
    pathsToLink = [ "/bin" ];
  };
  manifest = pkgs.writeText "neohaskell-components.json" (builtins.toJSON {
    schema = 1;
    system = pkgs.stdenv.hostPlatform.system;
    runtime = toString runtime;
    components = builtins.mapAttrs (_: component: {
      path = toString component;
    }) components;
  });
in {
  bundle = pkgs.linkFarm "neohaskell-ci-components" (
    map (name: { inherit name; path = components.${name}; }) names
    ++ [ { name = "manifest.json"; path = manifest; }
         { name = "runtime"; path = runtime; } ]);
}
