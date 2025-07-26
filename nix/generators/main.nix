{ pkgs }:
{ neoConfig }:
let
  inherit (neoConfig) name;
  mainModuleName = pkgs.lib.concatStrings (
    pkgs.lib.mapAttrsToList 
      (i: part: if i == 0 then part else pkgs.lib.toUpper (builtins.substring 0 1 part) + builtins.substring 1 (builtins.stringLength part) part)
      (pkgs.lib.imap0 (i: x: x) (pkgs.lib.splitString " " name))
  );
  
in ''
module Main where

import Core
import qualified Task
import qualified ${mainModuleName}

main :: IO ()
main = Task.runOrPanic ${mainModuleName}.run
''