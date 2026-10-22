import importlib.util
import sys
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "check-consumer-freshness.py"
SPEC = importlib.util.spec_from_file_location("consumer_freshness", SCRIPT)
FRESHNESS = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = FRESHNESS
SPEC.loader.exec_module(FRESHNESS)
ROOT = SCRIPT.parents[4]


class ConsumerFreshnessTests(unittest.TestCase):
    def setUp(self) -> None:
        self.defaults = FRESHNESS.marker_defaults(ROOT)

    def test_marker_ownership_tracks_the_th_generators(self) -> None:
        shared = {"Show", "Generic", "FromJSON", "ToJSON"}
        self.assertEqual(self.defaults["event"], shared)
        self.assertEqual(self.defaults["command"], shared | {"ToSchema"})
        self.assertEqual(self.defaults["deriveQuery"], shared | {"ToSchema"})
        self.assertEqual(self.defaults["deriveEntity"], {"Generic", "FromJSON", "ToJSON"})

    def test_reports_owned_deriving_and_qualified_empty_instances(self) -> None:
        source = """
import Data.Aeson qualified as Aeson
import GHC.Generics qualified as GHC
import Schema qualified as ApiSchema
import Service.CommandExecutor.TH qualified as CommandTH

data CreateCart = CreateCart
  deriving stock (Eq, GHC.Generic, Show, Typeable)

instance Aeson.FromJSON CreateCart
instance Aeson.ToJSON CreateCart
instance ApiSchema.ToSchema CreateCart

CommandTH.command ''CreateCart
"""
        findings = FRESHNESS.scan_source("Fixture.hs", source, self.defaults)
        self.assertEqual(
            {(item.class_name, item.declaration) for item in findings},
            {
                ("Generic", "deriving clause"),
                ("Show", "deriving clause"),
                ("FromJSON", "empty instance"),
                ("ToJSON", "empty instance"),
                ("ToSchema", "empty instance"),
            },
        )

    def test_reports_default_deriving_style_for_query_marker(self) -> None:
        source = """
data CounterView = CounterView
  deriving (Eq, Generic, Show)

instance ToSchema CounterView
deriveQuery ''CounterView []
"""
        findings = FRESHNESS.scan_source("Fixture.hs", source, self.defaults)
        self.assertEqual(
            {item.class_name for item in findings}, {"Generic", "Show", "ToSchema"}
        )

    def test_preserves_custom_json_methods(self) -> None:
        source = """
import Json qualified

data CartEvent = CartCreated

instance Json.ToJSON CartEvent where
  toJSON _ = Json.Null

instance Json.FromJSON CartEvent where
  parseJSON _ = pure CartCreated

deriveEvent ''CartEvent
"""
        self.assertEqual(FRESHNESS.scan_source("Fixture.hs", source, self.defaults), [])

    def test_finds_bare_deriving_and_empty_where_instances_through_core_alias(self) -> None:
        source = """
import Core qualified as Framework

data CounterEvent = CounterEvent deriving Framework.Generic

instance Framework.Show CounterEvent where
  show _ = "CounterEvent"

instance Framework.FromJSON CounterEvent where

instance Framework.ToJSON CounterEvent where
  toJSON _ = Framework.Null

event ''CounterEvent
"""
        findings = FRESHNESS.scan_source("Fixture.hs", source, self.defaults)
        self.assertEqual(
            {(item.class_name, item.declaration) for item in findings},
            {("Generic", "deriving clause"), ("FromJSON", "empty instance")},
        )

    def test_ignores_non_marker_types_and_non_owned_classes(self) -> None:
        source = """
data Manual = Manual
  deriving (Eq, Ord, Typeable, Generic, Show)
instance Json.ToJSON Manual

data CreateCart = CreateCart
  deriving (Eq, Typeable)
command ''CreateCart
"""
        findings = FRESHNESS.scan_source("Fixture.hs", source, self.defaults)
        self.assertEqual(findings, [])

    def test_ignores_markers_inside_comments_and_literals(self) -> None:
        source = r'''
import Json qualified

message = "command ''Ghost; data Ghost = Ghost deriving (Show, Generic)"
-- command ''LineComment
{- outer comment
   {- command ''NestedComment -}
   data Commented = Commented deriving (Show, Generic)
-}

data RealEvent = RealEvent
deriveEvent ''RealEvent
'''
        self.assertEqual(FRESHNESS.scan_source("Fixture.hs", source, self.defaults), [])


if __name__ == "__main__":
    unittest.main()
