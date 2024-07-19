module IO (IO, yield, dangerouslyRun) where

import GHC.IO (IO)
import System.IO.Unsafe qualified as GHC
import Prelude qualified


yield :: value -> IO value
yield = Prelude.pure


-- |
-- `dangerouslyRun` is an alias for `unsafePerformIO`, which allows running an IO action and extracting its result as a pure value. However, it comes with significant risks and should be used with extreme caution, especially for those new to Haskell. Here's why:
--
-- 1. __Unpredictable Execution Timing__: When you use `dangerouslyRun`, you're forcing an IO action to be run in a specific place. But the exact time it runs is hard to predict. This can lead to surprising behavior, like reading a file before it's written.
--
-- 2. __Potential for Race Conditions__: If you use `dangerouslyRun` in multiple threads, you might accidentally access the same mutable state (like global variables) at the same time. This can cause race conditions, where the threads interfere with each other and cause incorrect results.
--
-- 3. __Difficulty Reasoning about Code__: In other languages, you're used to side effects happening in a specific order. But with `dangerouslyRun`, side effects can happen in unexpected places. This makes it much harder to understand how the code works, both for you and others reading your code.
--
-- 4. __Hard to Test and Maintain__: Code using `dangerouslyRun` is harder to test because the results can change depending on things outside the function. If you want to change the code later, you might have to change all the places it's used, making maintenance harder.
--
-- In general, it's best to avoid `dangerouslyRun`.
dangerouslyRun :: IO a -> a
dangerouslyRun = GHC.unsafePerformIO
