module Neo (
  init,
) where

import Core
import Neo.Build qualified as Build


-- appConfig = App.config do
--   handle @SomeCommand someHandler
--   handle @SomeCommand someHandler

init :: Promise Void
init = Build.init