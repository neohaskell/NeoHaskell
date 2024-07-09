module Path (
  Path,
  RelativeTo,
  Absolute,
  File,
  Directory,
  toLinkedList,
  relativeFile,
  absoluteFile,
  relativeDirectory,
  absoluteDirectory,
) where

-- Import the reexported types and functions

-- Import qualified for using internally and renaming

import Char (Char)
import Language.Haskell.TH.Quote qualified
import LinkedList (LinkedList)
import "path" Path (File, Path)
import "path" Path qualified as HsPath


type RelativeTo = HsPath.Rel


type Absolute = HsPath.Abs


type Directory = HsPath.Dir


-- | Required to interact with legacy Haskell functions
toLinkedList :: Path to fileOrDir -> LinkedList Char
toLinkedList path = HsPath.toFilePath path


-- | Template string constructor for creating absolute paths to
-- directories.
--
-- Example:
--
-- > [absoluteDirectory|/home/user|]
absoluteDirectory :: Language.Haskell.TH.Quote.QuasiQuoter
absoluteDirectory = HsPath.absdir


-- | Template string constructor for creating relative paths to
-- directories.
--
-- Example:
--
-- > [relativeDirectory|/home/user|]
relativeDirectory :: Language.Haskell.TH.Quote.QuasiQuoter
relativeDirectory = HsPath.reldir


-- | Template string constructor for creating absolute paths to
-- files.
--
-- Example:
--
-- > [absoluteFile|/home/user/file.txt|]
absoluteFile :: Language.Haskell.TH.Quote.QuasiQuoter
absoluteFile = HsPath.absfile


-- | Template string constructor for creating relative paths to
-- files.
--
-- Example:
--
-- > [relativeFile|/home/user/file.txt|]
relativeFile :: Language.Haskell.TH.Quote.QuasiQuoter
relativeFile = HsPath.relfile