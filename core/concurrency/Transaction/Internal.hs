module Transaction.Internal (
  Transaction (..),
) where

import Control.Concurrent.STM qualified as GhcSTM
import Prelude qualified


newtype Transaction value = Transaction (GhcSTM.STM value)
  deriving newtype (Prelude.Functor, Prelude.Applicative, Prelude.Monad)
