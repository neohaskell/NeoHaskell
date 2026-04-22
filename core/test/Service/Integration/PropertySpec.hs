module Service.Integration.PropertySpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Property" do
    it "100 successful cases pass when the invariant is always true" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/1"

    it "failing invariant causes the property to fail" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/2"

    it "counter-example output includes the request (shrunk)" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/3"

    it "counter-example output never prints a Redacted field verbatim" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/4"

    it "fakeProperty shrinks the request (not the response)" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/5"

    it "failing invariant fails fast after the first shrink-minimal counter-example" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/6"

    it "user-supplied generator is respected when QuickCheck.forAll wraps fakeProperty" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/7"

    it "fakeProperty works with a Ping-typed response that has no Show (but request still does)" \_ ->
      pending "Phase 8 — ADR-0055 PropertySpec/8"
