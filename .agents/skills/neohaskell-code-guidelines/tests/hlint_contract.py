import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest


OWNER = Path(__file__).resolve().parents[1]
REPO = OWNER.parents[2]
HLINT = str(Path(os.environ.get("HLINT", shutil.which("hlint") or "hlint")).resolve())


class StaticChecks(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.owner = self.root / ".agents/skills/neohaskell-code-guidelines"
        (self.owner / "scripts").mkdir(parents=True)
        for script in (OWNER / "scripts").glob("*.py"):
            shutil.copy(script, self.owner / "scripts" / script.name)
        shutil.copy(REPO / ".hlint.yaml", self.root / ".hlint.yaml")
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)

    def source(self, text, name="core/Example.hs"):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def check(self, expected, *arguments):
        result = subprocess.run([sys.executable, str(self.owner / "scripts/check.py"), "--hlint", HLINT, *arguments], cwd=self.root, text=True, capture_output=True)
        self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
        return result.stdout + result.stderr

    def test_yielded_unit_conditionals_use_the_correct_task_operation(self):
        for imports, yielded in [
            ("import Task qualified\nimport Basics", "Task.yield unit"),
            ("import Task qualified as T\nimport Basics qualified as B", "T.yield B.unit"),
            ("import Task (yield)\nimport Core", "yield unit"),
            ("import Task qualified", "Task.yield ()"),
        ]:
            for expression, rule in [
                (f"if flag then {yielded} else action", "unless"),
                (f"if flag then action else {yielded}", "when"),
            ]:
                with self.subTest(yielded=yielded, rule=rule):
                    self.source(f"module Example where\n{imports}\nchoose flag action = {expression}\n")
                    self.assertIn(f"NeoHaskell: use Task.{rule}", self.check(1))
        for expression in ["Task.when flag action", "Task.unless flag action", "if flag then Task.yield value else action"]:
            self.source(f"module Example where\nimport Task qualified\nchoose flag action value = {expression}\n")
            self.check(0)
        self.source("module Example where\nimport Task qualified\nchoose flag action unit = if flag then Task.yield unit else action\n")
        self.check(0)

    def test_boolean_cases_require_known_constructors(self):
        for imported, prefix in [("import Basics", ""), ("import Core", ""), ("import Basics qualified as B", "B."), ("import Prelude qualified as P", "P.")]:
            for first, second in [("True", "False"), ("False", "True")]:
                self.source(f"module Example where\n{imported}\nchoose flag yes no = case flag of\n  {prefix}{first} -> yes\n  {prefix}{second} -> no\n")
                self.assertIn("NeoHaskell: use if for Bool", self.check(1))
        for source in [
            "import Other qualified as O\nchoose flag yes no = case flag of\n  O.True -> yes\n  O.False -> no\n",
            "import Basics hiding (Bool(..))\ndata Flag = True | False\nchoose flag yes no = case flag of\n  True -> yes\n  False -> no\n",
            "import Basics\nchoose flag yes no = if flag then yes else no\n",
            "import Basics\nchoose flag yes no = case flag of\n  True -> yes\n  _ -> no\n",
        ]:
            self.source("module Example where\n" + source)
            self.check(0)

    def test_fmt_conversion_requires_the_fmt_quasiquote(self):
        for imports, expression in [
            ("import Text qualified\nimport Basics (fmt)", "Text.fromLinkedList [fmt|hello|]"),
            ("import Text qualified as T\nimport Basics qualified as B", "T.fromLinkedList [B.fmt|hello #{name}|]"),
            ("import Text (fromLinkedList)\nimport Basics (fmt)", "fromLinkedList ([fmt|hello\nworld|])"),
        ]:
            self.source(f"module Example where\n{imports}\nrender name = {expression}\n")
            self.assertIn("NeoHaskell: fmt already produces Text", self.check(1))
        for expression in ['Text.fromLinkedList value', 'Text.fromLinkedList [sql|select 1|]', '[fmt|hello #{value}|]']:
            self.source(f"module Example where\nimport Text qualified\nimport Basics (fmt)\nrender value = {expression}\n")
            self.check(0)
        self.source("module Example where\nimport Text qualified\nimport Other (fmt)\nrender = Text.fromLinkedList [fmt|hello|]\n")
        self.check(0)

    def test_text_parameter_signatures_preserve_results_and_conversions(self):
        imports = "import LinkedList (LinkedList)\nimport Char (Char)\n"
        for signature in ["LinkedList Char -> Int", "Int -> LinkedList (Char) -> Int", "forall value. Show value => LinkedList Char -> value", "(LinkedList Char -> Text) -> Int"]:
            self.source(f"module Example where\n{imports}consume :: {signature}\n")
            self.assertIn("NeoHaskell: use Text parameters", self.check(1))
        for signature in ["Text -> LinkedList Char", "Text -> Result Error (LinkedList Char)", "(Text -> LinkedList Char) -> Int", "LinkedList Int -> Int", "Char LinkedList -> Int"]:
            self.source(f"module Example where\n{imports}produce :: {signature}\n")
            self.check(0)
        for path, module in [("core/core/Text.hs", "Text"), ("core/system/Path.hs", "Path")]:
            self.source(f"module {module} where\n{imports}fromLinkedList :: LinkedList Char -> Text\n", path)
        self.check(0)
        self.source(f"module Text where\n{imports}fromLinkedList :: LinkedList Char -> Text\n")
        self.assertIn("NeoHaskell: use Text parameters", self.check(1))

    def test_signature_aliases_comments_and_wrong_providers(self):
        for declaration in ["consume\n  :: LinkedList Char -> Int", "(<+>) :: LinkedList Char -> Int"]:
            self.source(f"module Example where\nimport LinkedList (LinkedList)\nimport Char (Char)\n{declaration}\n")
            self.assertIn("NeoHaskell: use Text parameters", self.check(1))
        for imported in ["import qualified LinkedList as L\nimport qualified Char as C", "import LinkedList qualified as L\nimport Char qualified as C"]:
            self.source(f"module Example where\n{imported}\nconsume ::\n  L.LinkedList C.Char -> Int\n")
            self.assertIn("NeoHaskell: use Text parameters", self.check(1))
        for imported, signature in [
            ("import Other qualified as O", "O.LinkedList O.Char -> Int"),
            ("import LinkedList qualified as L\nimport Char qualified as C", "LinkedList.LinkedList Char.Char -> Int"),
            ("import Char qualified as C", "C.LinkedList C.Char -> Int"),
            ("import LinkedList qualified as L", "L.LinkedList L.Char -> Int"),
        ]:
            self.source(f"module Example where\n{imported}\nconsume :: {signature}\n")
            self.check(0)
        self.source('module Example where\nimport LinkedList (LinkedList)\nimport Char (Char)\n-- consume :: LinkedList Char -> Int\nmessage = "consume :: LinkedList Char -> Int"\ndata Holder = Holder { value :: LinkedList Char }\n')
        self.check(0)

    def test_cabal_strict_defaults_and_source_overrides(self):
        self.source("cabal-version: 3.4\nname: fixture\nversion: 0.1\ncommon defaults\n  default-extensions: Strict\nlibrary\n  import: defaults\n  hs-source-dirs: core\n", "fixture.cabal")
        source = "module Example where\ndata Deferred value = Deferred (() -> value)\ndefer ~value = Deferred (\\_ -> value)\n"
        self.source(source)
        self.check(0)
        self.source("{-# LANGUAGE NoStrict #-}\n" + source)
        self.assertIn("Redundant irrefutable pattern", self.check(1))
        self.source("{-# LANGUAGE Strict, NoStrict #-}\n" + source)
        self.assertIn("Redundant irrefutable pattern", self.check(1))
        self.source("{-# LANGUAGE NoStrict, Strict #-}\n" + source)
        self.check(0)
        self.source(source + "first values = values !! 0\n")
        output = self.check(1)
        self.assertIn("core/Example.hs [first] Use head", output)
        self.assertNotIn("Redundant irrefutable pattern", output)

    def test_cabal_common_profiles_parser_flags_and_ambiguity(self):
        self.source("cabal-version: 3.4\nname: fixture\nversion: 0.1\ncommon syntax\n  default-extensions: Arrows\nlibrary\n  import: syntax\n  hs-source-dirs: core\n", "fixture.cabal")
        self.source("module Example where\nidentity = proc value -> returnA -< value\n")
        self.check(0)
        cabal = self.root / "fixture.cabal"
        cabal.write_text(cabal.read_text() + "test-suite other\n  hs-source-dirs: core\n")
        self.assertIn("Conflicting component", self.check(2))
        cabal.write_text("library\n  if flag(strict)\n    default-extensions: Strict\n  hs-source-dirs: core\n")
        self.assertIn("Conditional Cabal", self.check(2))

    def test_component_overrides_and_compiler_flag_precedence(self):
        self.source("common base\n  default-extensions: Strict\ncommon inherited\n  import: base\nlibrary\n  import: inherited\n  hs-source-dirs: strict\ntest-suite lazy\n  import: inherited\n  ghc-options: -XNoStrict\n  default-extensions: Strict\n  hs-source-dirs: lazy\n", "fixture.cabal")
        source = "module Example where\ndata Deferred value = Deferred (() -> value)\ndefer ~value = Deferred (\\_ -> value)\n"
        self.source(source, "strict/Example.hs")
        self.check(0)
        self.source(source, "lazy/Example.hs")
        output = self.check(1)
        self.assertIn("lazy/Example.hs [defer] Redundant irrefutable pattern", output)
        self.assertNotIn("FAIL strict/Example.hs", output)

    def test_strict_normalization_preserves_multiline_source_locations(self):
        self.source("library\n  default-extensions: Strict\n  hs-source-dirs: core\n", "fixture.cabal")
        self.source("{-# LANGUAGE\n  Strict, QuasiQuotes\n #-}\nmodule Example where\nfirst values = values !! 0\n")
        self.assertIn("core/Example.hs [first] Use head (1): first values = values !! 0", self.check(1))
        self.source("{- Outer comment {-# LANGUAGE Strict #-} -}\n{-# LANGUAGE NoStrict #-}\nmodule Example where\ndata Deferred value = Deferred (() -> value)\ndefer ~value = Deferred (\\_ -> value)\n")
        self.assertIn("Redundant irrefutable pattern", self.check(1))
        self.source("{-# LANGUAGE NoStrict #-}\n--- A comment containing {-# LANGUAGE Strict #-}\nmodule Example where\ndata Deferred value = Deferred (() -> value)\ndefer ~value = Deferred (\\_ -> value)\n")
        self.assertIn("Redundant irrefutable pattern", self.check(1))

    def test_unsupported_language_configuration_fails_visibly(self):
        for cabal, expected in [
            ("common cycle\n  import: cycle\nlibrary\n  import: cycle\n  hs-source-dirs: core\n", "Invalid common stanza"),
            ("library\n  hs-source-dirs: elsewhere\n", "No Cabal component"),
        ]:
            self.source(cabal, "fixture.cabal")
            self.source("module Example where\nvalue = 1\n")
            self.assertIn(expected, self.check(2))
        self.source("library\n  hs-source-dirs: core\n", "fixture.cabal")
        for pragma, expected in [("{-# OPTIONS_GHC -XStrict #-}", "OPTIONS_GHC"), ('{-# LINE 20 "generated.hs" #-}', "LINE pragmas")]:
            self.source(pragma + "\nmodule Example where\nvalue = 1\n")
            self.assertIn(expected, self.check(2))

    def test_starter_inherits_its_owned_cabal_template(self):
        self.source("common defaults\n  default-extensions: Strict\nlibrary\n  import: defaults\n  hs-source-dirs: src\n", "neo/assets/templates/project.cabal.j2")
        self.source("module Example where\ndata Deferred value = Deferred (() -> value)\ndefer ~value = Deferred (\\_ -> value)\n", "neo/starter/src/Example.hs")
        self.check(0)

    def test_new_rules_are_not_disabled_by_local_suppression(self):
        for rule, source in [
            ("NeoHaskell: use Task.unless for yielded unit", "import Task qualified\nimport Basics\nchoose flag action = if flag then Task.yield unit else action\n"),
            ("NeoHaskell candidate: Boolean case", "import Basics\nchoose flag yes no = case flag of\n  True -> yes\n  False -> no\n"),
            ("NeoHaskell candidate: Text conversion", "import Basics (fmt)\nimport Text qualified\nrender = Text.fromLinkedList [fmt|hello|]\n"),
        ]:
            self.source("module Example where\n" + source + f'{{-# ANN module ("HLint: ignore {rule}" :: String) #-}}\n')
            self.check(1)

    def test_literate_default_profile_preserves_diagnostic_identity(self):
        self.source("> module Example where\n> first values = values !! 0\n", "core/Example.lhs")
        self.assertIn("core/Example.lhs [first] Use head", self.check(1))
        self.source("> module Example where\n> value = 1\n", "core/Example.lhs")
        self.check(0)

    def test_literate_custom_rules_check_code_and_ignore_prose(self):
        for code in [
            "import LinkedList (LinkedList)\nimport Char (Char)\nconsume :: LinkedList Char -> Int\n",
            "import Basics\nchoose flag yes no = case flag of\n  True -> yes\n  False -> no\n",
            "import Basics (fmt)\nimport Text qualified\nrender = Text.fromLinkedList [fmt|hello|]\n",
        ]:
            source = "module Example where\n" + code
            for literate in ["\n".join("> " + line for line in source.splitlines()) + "\n", "\\begin{code}\n" + source + "\\end{code}\n"]:
                self.source(literate, "core/Example.lhs")
                self.check(1)
        self.source("A prose example. consume :: LinkedList Char -> Int\n\n> module Example where\n> value = 1\n", "core/Example.lhs")
        self.check(0)

    def test_indented_module_bodies_keep_rule_coverage(self):
        for body in [
            "import LinkedList (LinkedList)\nimport Char (Char)\nconsume :: LinkedList Char -> Int\n",
            "import Basics\nchoose flag yes no = case flag of\n  True -> yes\n  False -> no\n",
            "import Basics (fmt)\nimport Text qualified\nrender = Text.fromLinkedList [fmt|hello|]\n",
        ]:
            indented = "\n".join("  " + line for line in body.splitlines()) + "\n"
            self.source("module Example where\n" + indented)
            self.check(1)
        self.source("module Example where\n  import LinkedList (LinkedList)\n  import Char (Char)\n  produce :: Text -> LinkedList Char\n  data Holder = Holder { value :: LinkedList Char }\n")
        self.check(0)

    def test_transaction_constructor_ownership_is_exact(self):
        for module in ["Transaction", "AtomicVar", "ConcurrentMap", "Service.Example", "Auth.Example", "Transaction.Extra", "TaskSpec"]:
            self.source(f"module {module} where\nimport Transaction.Internal (Transaction(..))\nwrap value = Transaction value\n")
            self.check(0 if module in {"Transaction", "AtomicVar", "ConcurrentMap"} else 1)
        for module in ["Transaction", "Transaction.Internal", "Transaction.Extra"]:
            self.source(f"module {module} where\nimport Control.Concurrent.STM qualified as Raw\ntype Action = Raw.STM ()\n")
            self.check(1 if module == "Transaction.Extra" else 0)

    def test_core_alternatives_pass(self):
        self.source("module Example where\nimport Task qualified\nimport Var qualified\nreadValue variable = Var.get variable\ntranslate task = Task.mapError explain task\n")
        self.check(0)

    def test_qualified_and_unqualified_exception_calls_fail(self):
        self.source("module Example where\nimport Control.Exception qualified as Runtime\nfirst action = Runtime.mask_ action\n")
        self.assertIn("Avoid restricted function", self.check(1))
        self.source("module Example where\nimport Control.Exception (mask_)\nfirst action = mask_ action\n")
        self.assertIn("Avoid restricted function", self.check(1))

    def test_owner_and_unrelated_operation_pass(self):
        for module, imported, expression in [
            ("Task", "Control.Exception", "Runtime.mask_ action"),
            ("AsyncTask", "GHC.Conc", "Runtime.threadStatus action"),
            ("Var", "Data.IORef", "Runtime.readIORef action"),
        ]:
            with self.subTest(module=module):
                self.source(f"module {module} where\nimport {imported} qualified as Runtime\noperation action = {expression}\n")
                self.check(0)
        self.source("module Example where\nimport Task qualified\nraise error = Task.throw error\n")
        self.check(0)

    def test_io_exception_owner_passes(self):
        for imported in ["Control.Exception", "Control.Exception.Base"]:
            for qualified in [True, False]:
                with self.subTest(imported=imported, qualified=qualified):
                    import_names = "qualified as Runtime" if qualified else "(finally, onException, catch, try)"
                    prefix = "Runtime." if qualified else ""
                    self.source(f"module IO where\nimport {imported} {import_names}\nfinish action cleanup = {prefix}finally action cleanup\ncleanFailed action cleanup = {prefix}onException action cleanup\nrecover action handler = {prefix}catch action handler\nattempt action = {prefix}try action\n", "core/core/IO.hs")
                    self.check(0)

    def test_application_exception_operations_fail(self):
        for imported in ["Control.Exception", "Control.Exception.Base"]:
            with self.subTest(imported=imported):
                self.source(f"module Example where\nimport {imported} qualified as Runtime\nfinish action cleanup = Runtime.finally action cleanup\n")
                output = self.check(1)
                self.assertIn("Avoid restricted module", output)
                self.assertIn("Avoid restricted function", output)

    def test_io_does_not_own_mutable_state_or_thread_inspection(self):
        for imported, expression in [
            ("Data.IORef", "Runtime.readIORef value"),
            ("GHC.Conc", "Runtime.threadStatus value"),
            ("GHC.Conc.Sync", "Runtime.threadStatus value"),
        ]:
            with self.subTest(imported=imported):
                self.source(f"module IO where\nimport {imported} qualified as Runtime\noperation value = {expression}\n", "core/core/IO.hs")
                output = self.check(1)
                self.assertIn("Avoid restricted module", output)
                self.assertIn("Avoid restricted function", output)

    def test_internal_exception_type_import_ownership(self):
        for module in ["Service.Example", "Task", "AsyncTask", "IO", "Directory", "File", "Subprocess"]:
            for qualified in [True, False]:
                with self.subTest(module=module, qualified=qualified):
                    imported = "qualified as Runtime" if qualified else "(IOError)"
                    value = "Runtime.IOError" if qualified else "IOError"
                    self.source(f"module {module} where\nimport GHC.IO.Exception {imported}\ntype Failure = {value}\n")
                    output = self.check(1 if module == "Service.Example" else 0)
                    if module == "Service.Example":
                        self.assertIn("Avoid restricted module", output)
        self.source("module Service.Example where\nimport GHC.TypeLits (Nat)\ntype Size = Nat\n")
        self.check(0)

    def test_internal_and_public_exception_operation_ownership(self):
        operations = {
            "GHC.IO.Exception": ["ioException", "ioError", "assertError"],
            "System.IO.Error": ["ioError", "catchIOError", "tryIOError", "modifyIOError"],
        }
        for module in ["Service.Example", "Directory", "File", "Subprocess", "Task", "AsyncTask", "IO"]:
            for imported, names in operations.items():
                for operation in names:
                    for qualified in [True, False]:
                        with self.subTest(module=module, imported=imported, operation=operation, qualified=qualified):
                            import_names = "qualified as Runtime" if qualified else f"({operation})"
                            prefix = "Runtime." if qualified else ""
                            self.source(f"module {module} where\nimport {imported} {import_names}\noperation value = {prefix}{operation} value\n")
                            expected = 0 if module in {"Task", "AsyncTask", "IO"} else 1
                            output = self.check(expected)
                            if expected:
                                self.assertIn("Avoid restricted function", output)

    def test_internal_prelude_has_no_owner_exception(self):
        for module in ["Service.Example", "Task", "AsyncTask", "IO", "Directory", "File", "Subprocess", "DirectorySpec"]:
            with self.subTest(module=module):
                self.source(f"module {module} where\nimport System.Directory.Internal.Prelude qualified as Runtime\noperation value = Runtime.try value\n")
                self.assertIn("Avoid restricted module", self.check(1))

    def test_suppression_does_not_hide_internal_exception_rules(self):
        cases = [
            ("Service.Example", "GHC.IO.Exception", "type Failure = Runtime.IOError", "Avoid restricted module"),
            ("Directory", "GHC.IO.Exception", "operation value = Runtime.ioException value", "Avoid restricted function"),
            ("Directory", "System.IO.Error", "operation value = Runtime.tryIOError value", "Avoid restricted function"),
            ("Directory", "System.Directory.Internal.Prelude", "operation value = Runtime.try value", "Avoid restricted module"),
        ]
        for module, imported, declaration, rule in cases:
            for suppression in [f'{{-# ANN module ("HLint: ignore {rule}" :: String) #-}}', f'{{- HLINT ignore "{rule}" -}}']:
                with self.subTest(imported=imported, suppression=suppression):
                    self.source(f"module {module} where\nimport {imported} qualified as Runtime\n{suppression}\n{declaration}\n")
                    self.assertIn(rule, self.check(1))

    def test_public_error_classifiers_pass(self):
        for module in ["Directory", "File", "Subprocess"]:
            with self.subTest(module=module):
                self.source(f"module {module} where\nimport System.IO.Error qualified as Runtime\ntype Failure = Runtime.IOError\nmissing value = Runtime.isDoesNotExistError value\nclassify value = Runtime.ioeGetErrorType value\n")
                self.check(0)

    def test_filesystem_oracle_allowance_is_exact(self):
        for module in ["DirectorySpec", "DirectorySpec.Extra", "OtherSpec", "Service.Example"]:
            with self.subTest(module=module):
                self.source(f"module {module} where\nimport System.Directory qualified as Filesystem\nexists path = Filesystem.doesPathExist path\n")
                output = self.check(0 if module == "DirectorySpec" else 1)
                if module != "DirectorySpec":
                    self.assertIn("Avoid restricted module", output)

    def test_explicit_strict_preserves_lazy_pattern_without_hiding_other_hints(self):
        source = "module Example where\ndata Deferred value = Deferred (() -> value)\ndefer ~value = Deferred (\\_ -> value)\n"
        self.source(source)
        self.assertIn("Redundant irrefutable pattern", self.check(1))
        self.source("{-# LANGUAGE Strict #-}\n" + source)
        self.check(0)
        self.source("{-# LANGUAGE Strict #-}\n" + source + "first values = values !! 0\n")
        output = self.check(1)
        self.assertIn("Use head", output)
        self.assertNotIn("Redundant irrefutable pattern", output)

    def test_tracked_repeated_io_ref_use_fails(self):
        self.source("module Example where\nimport Data.IORef qualified as Raw\nreadBoth ref = do\n  Raw.readIORef ref\n  Raw.readIORef ref\n")
        subprocess.run(["git", "add", "core/Example.hs"], cwd=self.root, check=True)
        self.assertIn("Avoid restricted function (2)", self.check(1))

    def test_dialect_module_restriction_still_fails(self):
        self.source("module Example where\nimport Data.List qualified as Raw\nvalue = Raw.reverse [1, 2]\n")
        self.assertIn("Avoid restricted module", self.check(1))

    def test_suppression_does_not_hide_error_translation(self):
        self.source('module Example where\nimport Task qualified\n{-# ANN module ("HLint: ignore NeoHaskell: use Task.mapError" :: String) #-}\ntranslate task = Task.recover (\\error -> Task.throw (explain error)) task\n')
        self.assertIn("NeoHaskell: use Task.mapError", self.check(1))

    def test_old_restriction_suppression_is_preserved(self):
        self.source('module Example where\nimport Data.List qualified as Raw\n{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}\nvalue = Raw.reverse [1, 2]\n')
        self.check(0)

    def test_thread_status_requires_owner(self):
        self.source("module Example where\nimport GHC.Conc qualified as Runtime\nstatus thread = Runtime.threadStatus thread\n")
        self.assertIn("Avoid restricted function", self.check(1))

    def test_raw_async_and_stm_operations_fail_in_services(self):
        cases = [
            ("Control.Concurrent.Async", "qualified as Raw", "spawn task = Raw.async task"),
            ("Control.Concurrent.Async", "(async)", "spawn task = async task"),
            ("Control.Concurrent.STM", "qualified as Raw", "readValue value = Raw.readTVar value\ncommit value = Raw.atomically value"),
            ("Control.Concurrent.STM", "(atomically, readTVar)", "readValue value = readTVar value\ncommit value = atomically value"),
        ]
        for module in ["Service.Example", "Auth.Example"]:
            for imported, names, operation in cases:
                with self.subTest(module=module, imported=imported, names=names):
                    self.source(f"module {module} where\nimport {imported} {names}\n{operation}\n")
                    self.assertIn("Avoid restricted module", self.check(1))

    def test_raw_concurrency_type_imports_and_reexports_fail(self):
        cases = [
            "module Service.Example where\nimport Control.Concurrent.Async (Async)\ntype Worker = Async ()\n",
            "module Auth.Example where\nimport Control.Concurrent.STM (STM)\ntype Transaction = STM ()\n",
            "module Service.Example (module Control.Concurrent.Async) where\nimport Control.Concurrent.Async\n",
            "module Auth.Example (module Control.Concurrent.STM) where\nimport Control.Concurrent.STM\n",
        ]
        for source in cases:
            with self.subTest(source=source):
                self.source(source)
                self.assertIn("Avoid restricted module", self.check(1))

    def test_alternate_raw_concurrency_modules_fail(self):
        cases = [
            ("Control.Concurrent.Async.Internal", "use value = Raw.async value"),
            ("Control.Monad.STM", "use value = Raw.atomically value"),
            ("Control.Concurrent.STM.TVar", "use value = Raw.readTVar value"),
            ("Control.Concurrent.STM.TMVar", "use value = Raw.readTMVar value"),
            ("Control.Concurrent.STM.TChan", "use value = Raw.readTChan value"),
            ("Control.Concurrent.STM.TQueue", "use value = Raw.readTQueue value"),
            ("Control.Concurrent.STM.TArray", "array :: Raw.TArray Int Int -> Int\narray _ = 0"),
        ]
        for imported, declaration in cases:
            with self.subTest(imported=imported):
                self.source(f"module Service.Example where\nimport {imported} qualified as Raw\n{declaration}\n")
                self.assertIn("Avoid restricted module", self.check(1))

    def test_all_six_concurrency_owner_pairs_pass(self):
        cases = [
            ("AsyncTask", "Control.Concurrent.Async", "Raw.async value"),
            ("Subprocess", "Control.Concurrent.Async", "Raw.async value"),
            ("AtomicVar", "Control.Concurrent.STM", "Raw.readTVar value"),
            ("Channel", "Control.Concurrent.STM", "Raw.atomically value"),
            ("ConcurrentMap", "Control.Concurrent.STM", "Raw.atomically value"),
            ("Channel", "Control.Concurrent.STM.TBQueue", "Raw.readTBQueue value"),
        ]
        for module, imported, operation in cases:
            with self.subTest(module=module, imported=imported):
                self.source(f"module {module} where\nimport {imported} qualified as Raw\nrun value = {operation}\n")
                self.check(0)

    def test_cross_owner_near_name_dispatcher_and_task_spec_imports_fail(self):
        cases = [
            ("AtomicVar", "Control.Concurrent.Async", "Raw.async value"),
            ("AsyncTask", "Control.Concurrent.STM", "Raw.atomically value"),
            ("ConcurrentMap", "Control.Concurrent.STM.TBQueue", "Raw.readTBQueue value"),
            ("AsyncTask.Extra", "Control.Concurrent.Async", "Raw.async value"),
            ("Channel.Extra", "Control.Concurrent.STM.TBQueue", "Raw.readTBQueue value"),
            ("Service.Integration.Dispatcher", "Control.Concurrent.STM", "Raw.atomically value"),
            ("Service.Integration.Dispatcher", "Control.Concurrent.Async", "Raw.async value"),
            ("TaskSpec", "Control.Concurrent.Async", "Raw.async value"),
        ]
        for module, imported, operation in cases:
            with self.subTest(module=module, imported=imported):
                self.source(f"module {module} where\nimport {imported} qualified as Raw\nrun value = {operation}\n")
                self.assertIn("Avoid restricted module", self.check(1))
        self.source("module Service.Example where\nimport Control.Concurrent.STM.TBQueue qualified as Raw\nrun value = Raw.readTBQueue value\n")
        self.assertIn("Avoid restricted module", self.check(1))

    def test_concurrency_ownership_suppressions_remain_rejected(self):
        cases = [
            ("Service.Example", "Control.Concurrent.Async", "run value = Raw.async value"),
            ("Auth.Example", "Control.Concurrent.STM", "readValue value = Raw.readTVar value\ncommit value = Raw.atomically value"),
        ]
        suppressions = [
            '{-# ANN module ("HLint: ignore Avoid restricted module" :: String) #-}',
            '{- HLINT ignore "Avoid restricted module" -}',
        ]
        for module, imported, operations in cases:
            for suppression in suppressions:
                with self.subTest(module=module, suppression=suppression):
                    self.source(f"module {module} where\nimport {imported} qualified as Raw\n{suppression}\n{operations}\n")
                    self.assertIn("Avoid restricted module", self.check(1))

    def test_core_concurrency_apis_pass(self):
        self.source("module Service.Example where\nimport AsyncTask qualified\nimport AtomicVar qualified\nimport Channel qualified\nimport ConcurrentMap qualified\nspawn task = AsyncTask.run task\nnewVariable value = AtomicVar.containing value\nnewChannel = Channel.new\nnewMap = ConcurrentMap.new\n")
        self.check(0)

    def test_hlint_concurrency_ownership_limits_are_explicit(self):
        spoof = self.source("module AsyncTask where\nimport Control.Concurrent.Async qualified as Raw\nspawn action = Raw.async action\n", "services/Billing.hs")
        self.check(0)
        spoof.unlink()
        self.source("module Service.Example where\nimport StmContainers.Map qualified as STMMap\nnewMap = STMMap.newIO\n")
        self.check(0)

    def test_error_translation_uses_task_combinator(self):
        for expression in [
            "Task.recover (\\error -> Task.throw (explain error)) task",
            "task |> Task.recover (\\error -> Task.throw (explain error))",
            "task |> Task.recover (\\error -> explain error |> Task.throw)",
            "task |> Task.recover (\\error -> error |> explain |> Task.throw)",
            "Task.recover (\\error -> Task.throw (explain error))",
        ]:
            with self.subTest(expression=expression):
                self.source(f"module Example where\nimport Basics\nimport Task qualified\ntranslate task = {expression}\n")
                self.assertIn("NeoHaskell: use Task.mapError", self.check(1))
        self.source("module Example where\nimport Task qualified\ntranslate task = Task.mapError explain task\n")
        self.check(0)
        self.source("module Example where\nimport Basics\nimport Task qualified\ntranslate task = task |> Task.mapError explain\n")
        self.check(0)
        self.source("module Example where\nimport Basics\nimport Task qualified\nrecover task = task |> Task.recover retry\n")
        self.check(0)

    def test_suppression_does_not_hide_ownership_rule(self):
        self.source('module Example where\nimport Control.Exception qualified as Runtime\n{-# ANN module ("HLint: ignore Avoid restricted function" :: String) #-}\nprotect action = Runtime.mask_ action\n')
        self.assertIn("Avoid restricted function", self.check(1))

    def test_existing_non_task_false_positive_suppression_is_preserved(self):
        self.source('module Example where\n{-# ANN module ("HLint: ignore NeoHaskell: use Task.when" :: String) #-}\npass :: IO ()\npass = pure ()\nrun :: Bool -> IO () -> IO ()\nrun flag action = if flag then action else pass\n')
        self.check(0)

    def test_workspace_reference_is_rejected_in_haskell_and_python(self):
        for name, source in [
            ("core/Example.hs", 'module Example where\nfolder = ".agents-state/test"\n'),
            ("core/test/check.py", 'folder = ".agents-state/logs"\n'),
        ]:
            with self.subTest(name=name):
                path = self.source(source, name)
                self.assertIn("Agent workspace in runtime source", self.check(1))
                path.unlink()

    def test_scoped_resource_and_agent_tooling_pass(self):
        self.source('module Example where\nimport Directory qualified\nwithFolder action = Directory.withTemporary action\n')
        self.source('folder = ".agents-state/logs"\n', ".agents/skills/example/scripts/check.py")
        self.check(0)

    def test_parse_error_cannot_be_accepted(self):
        self.source("module Example where\nvalue = (\n")
        self.assertIn("Parse error", self.check(2))

    def test_invalid_hlint_output_fails_visibly(self):
        self.source("module Example where\nvalue = 1\n")
        executable = self.root / "fake-hlint"
        for output, status in [("not JSON", 0), ("{}", 0), ("[]", 1)]:
            with self.subTest(output=output, status=status):
                executable.write_text(f"#!{sys.executable}\nimport sys\nif '--numeric-version' in sys.argv:\n    print('3.10')\nelse:\n    print({output!r})\n    sys.exit({status})\n")
                executable.chmod(0o755)
                self.assertIn("HLint", self.check(2, "--hlint", str(executable)))

    def test_existing_suggestion_gate_is_preserved(self):
        self.source("module Example where\nfirst values = values !! 0\n")
        self.assertIn("Use head", self.check(1))


if __name__ == "__main__":
    unittest.main()
