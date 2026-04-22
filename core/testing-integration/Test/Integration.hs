-- | Umbrella module that re-exports the test-only helpers. Keep this the
-- only import path for test code: if @Test.Integration@ is the boundary,
-- production modules cannot reach 'simulate', 'record', 'promote', or
-- 'fakeProperty' even by accident.
module Test.Integration
  ( module Re,
  )
where

import Test.Integration.Contract as Re (contractTests)
import Test.Integration.EntropyScan as Re (scanForSecrets, secretPrefixes, shannonEntropy)
import Test.Integration.Fixture as Re (record, promote, RedactionRule (..), defaultRedactionRules, applyRedactionRules)
import Test.Integration.Property as Re (fakeProperty)
import Test.Integration.Simulate as Re (simulate)
