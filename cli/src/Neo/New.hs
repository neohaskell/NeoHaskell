module Neo.New (
  handle,
  Error (..),
  ProjectName (..),
) where

import Neo.Core


newtype ProjectName = ProjectName Text
  deriving (Show, Eq, Ord)


data Error
  = CabalFileError
  | CustomError Text
  deriving (Show)


handle :: ProjectName -> Task Error Unit
handle (ProjectName _) = do
  print "Hello world"
