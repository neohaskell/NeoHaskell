---
group: platform
component: Framework
impact: compatible
category: Fixed
---

## Summary

`actual |> shouldContain expected` now checks whether the actual array contains the expected subarray. `actual |> shouldNotContain expected` checks whether the actual array does not contain it. Both matchers previously checked the arrays in the opposite direction.

If you swapped the arguments to work around this bug, put the expected subarray first again. For example, change `expected |> shouldContain actual` to `actual |> shouldContain expected`. Run your tests to confirm that their assertions still check the intended array.
