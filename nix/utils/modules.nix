{ pkgs }:
{
  discoverModules = srcPath:
    if builtins.pathExists srcPath then
      let
        walkDir = dir: prefix:
          let
            fullSrcPath = if dir == "" then srcPath else (srcPath + "/" + dir);
            entries = builtins.readDir fullSrcPath;
            processEntry = name: type:
              let
                fullPath = if dir == "" then name else "${dir}/${name}";
                moduleName = if prefix == "" then name else "${prefix}.${name}";
              in
                if type == "directory" then
                  walkDir fullPath moduleName
                else if type == "regular" && pkgs.lib.hasSuffix ".hs" name then
                  [ (pkgs.lib.removeSuffix ".hs" moduleName) ]
                else [];
          in
            pkgs.lib.flatten (pkgs.lib.mapAttrsToList processEntry entries);
      in
        walkDir "" ""
    else [];
}