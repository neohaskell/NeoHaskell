module Neo.Core.Cabal () where

import Core
import Distribution.Compat.Prelude qualified as Either
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint (writePackageDescription)
import Distribution.SPDX (License (..))
import Distribution.Simple.Utils
import Distribution.Types.PackageName
import Distribution.Version


generateCabalFile :: IO ()
generateCabalFile = do
  let pkgName = mkPackageName "my-package"
  let version = mkVersion [0, 1, 0, 0]

  let pkg =
        emptyPackageDescription
          { package = PackageIdentifier pkgName version,
            licenseRaw = Either.Left NONE,
            synopsis = "A short description of the package",
            description = "A longer description of the package",
            category = "Development",
            maintainer = "your@email.com",
            author = "Your Name",
            stability = "Experimental"
          }
  let lib = emptyLibrary
  -- { libBuildInfo =
  --     emptyBuildInfo
  --       { hsSourceDirs = ["src"],
  --         exposedModules = ["MyLib"]
  --       }
  -- }

  let exe =
        emptyExecutable
          { exeName = mkUnqualComponentName "my-exe",
            modulePath = "app/Main.hs",
            buildInfo =
              emptyBuildInfo
                { hsSourceDirs = ["app"],
                  otherModules = ["Paths_my_package"]
                }
          }

  let finalPkg =
        pkg
          { library = Just lib,
            executables = [exe],
            version = version
          }

  writePackageDescription "my-package.cabal" finalPkg