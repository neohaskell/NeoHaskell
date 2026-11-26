{ pkgs, frameworkSource, frameworkRevision }:
let
  starter = import ./hix.nix { inherit pkgs; };
  compiler = pkgs.haskell-nix.resolve-compiler-name starter.compiler-nix-name;
  frameworkGitUrl = "https://github.com/neohaskell/neohaskell.git";
  ghc = pkgs.haskell-nix.compiler.${compiler};
  tools = pkgs.haskell-nix.tools compiler starter.shell.tools;
  hls = tools.haskell-language-server;
  hlsWrapper = hls.project.getComponent
    "haskell-language-server:exe:haskell-language-server-wrapper";
  editorTools = pkgs.runCommand "neo-editor-tools" { } ''
    mkdir -p "$out/bin"
    ln -s ${hlsWrapper}/bin/haskell-language-server-wrapper "$out/bin/"
    ln -s ${hls}/bin/haskell-language-server "$out/bin/haskell-language-server-${ghc.version}"
  '';

  docs = pkgs.haskell-nix.cabalProject' {
    src = builtins.path { path = ./toolchain-docs; name = "neo-toolchain-docs"; };
    compiler-nix-name = compiler;
    cabalProject = ''
      packages: *.cabal

      source-repository-package
        type: git
        location: ${frameworkGitUrl}
        tag: ${frameworkRevision}
        subdir: core

      source-repository-package
        type: git
        location: ${frameworkGitUrl}
        tag: ${frameworkRevision}
        subdir: integrations

      constraints:
        hasql < 1.10,
        opt-env-conf < 0.11
    '';
    inputMap."${frameworkGitUrl}/${frameworkRevision}" = frameworkSource;
  };
  docsGhc = (docs.shellFor {
    packages = ps: [ ps.neo-toolchain-docs ];
    tools.hoogle = "latest";
    withHoogle = true;
  }).ghc;
  hoogleTool = docs.tool "hoogle" "latest";
  hoogle = pkgs.runCommand "neo-hoogle" { } ''
    mkdir -p "$out/bin" "$out/share/doc"
    ln -s ${docsGhc}/bin/hoogle "$out/bin/hoogle"
    ln -s ${pkgs.lib.getOutput "doc" ghc}/share/doc/ghc "$out/share/doc/ghc"
  '';

  bundle = pkgs.symlinkJoin {
    name = "neo-toolchain";
    paths = builtins.attrValues tools ++ [ ghc editorTools hoogle ];
    postBuild = ''
      mkdir -p "$out/share/neohaskell/plans"
      ln -s ${docs.plan-nix} "$out/share/neohaskell/plans/documentation"
      ln -s ${hoogleTool.project.plan-nix} "$out/share/neohaskell/plans/hoogle"
      ${pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (name: tool:
        ''ln -s ${tool.project.plan-nix} "$out/share/neohaskell/plans/${name}"''
      ) tools)}
      for tool in ghc cabal haddock hoogle hspec-discover hlint fourmolu haskell-language-server haskell-language-server-wrapper; do
        test -x "$out/bin/$tool"
      done
    '';
  };
in {
  inherit bundle compiler editorTools hoogle;
  toolVersions = starter.shell.tools;
}
