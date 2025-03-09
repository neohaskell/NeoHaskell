module Neo.Build.Templates.Cabal (
  template,
) where

import Array qualified
import Map qualified
import Neo.Core
import Text qualified
import Version qualified


-- FIXME: Bring here discovered modules in the folders
template :: ProjectConfiguration -> Array Text -> Text
template
  ProjectConfiguration {name, version, description, license, author, dependencies}
  modules = do
    let execName = Text.toKebabCase name
    -- FIXME: Move onto a separate version handling module
    let vText = Version.toText version
    let makeDep (k, v)
          | v |> Text.trim |> Text.startsWith "^" = [fmt|{k} ^>= {v |> Text.replace "^" ""}|]
          | otherwise = [fmt|{k} == {v}|]
    let deps =
          dependencies
            |> Map.entries
            |> Array.map makeDep
            |> Array.push [fmt|nhcore|]
            |> Text.joinWith ", "

    let mods = modules |> Text.joinWith ", "
    [fmt|cabal-version:      3.4
-- THIS CABAL FILE IS AUTOGENERATED BY `neo`, THE NEOHASKELL CLI TOOL.
-- YOU SHOULD NOT MODIFY IT, AS IT WILL BE REGENERATED NEXT TIME `neo` RUNS.
--
-- IF YOU HAVE A SPECIFIC NEED TO MODIFY THIS
-- FILE, PLEASE STATE SO EITHER IN A GITHUB
-- ISSUE: https://github.com/NeoHaskell/NeoHaskell/issues
-- OR IN THE NEOHASKELL DISCORD SERVER.
-- YOU CAN JOIN IT THROUGH THE LINK IN
-- https://neohaskell.org
name:               {name}
version:            {vText}
synopsis:           {description}
license:            {license}
author:             {author}

common common_cfg
    ghc-options:    -Wall
                    -Werror
                    -threaded
    default-language: GHC2021
    default-extensions:
      ApplicativeDo
      BlockArguments
      DataKinds
      NoImplicitPrelude
      TemplateHaskell
      DeriveDataTypeable
      QuasiQuotes
      QualifiedDo
      ImpredicativeTypes
      ImportQualifiedPost
      OverloadedStrings
      OverloadedLabels
      OverloadedRecordDot
      DuplicateRecordFields
      PackageImports
      NamedFieldPuns
      Strict
      TypeFamilies

    build-depends:
      {deps}

library
    import:           common_cfg
    exposed-modules:
      {mods}
    hs-source-dirs:   src

executable {execName}
    import:           common_cfg
    main-is:          Main.hs
    build-depends:
        {name}
    hs-source-dirs:   app

  |]
