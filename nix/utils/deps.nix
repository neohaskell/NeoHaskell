{ pkgs }:
{
  # Process dependency specifications from neo.json
  processDeps = dependencies:
    let
      makeDep = dep: version:
        if pkgs.lib.hasPrefix "^" version then
          "${dep} ^>= ${pkgs.lib.removePrefix "^" version}"
        else
          "${dep} == ${version}";
    in
      pkgs.lib.mapAttrsToList makeDep dependencies;
      
  # Convert dependency list to cabal format string
  depsToString = dependencies:
    let
      processed = (import ./deps.nix { inherit pkgs; }).processDeps dependencies;
      withNhcore = processed ++ ["nhcore"];
    in
      pkgs.lib.concatStringsSep ", " withNhcore;
}