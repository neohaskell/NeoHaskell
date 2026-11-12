---
group: platform
component: Framework
impact: none
category: Improved
---

## Summary

Framework users need no app changes. Cache publication verification accounts for dependencies served by the configured upstream public caches. Cachix avoids storing duplicate upstream dependencies, so checking only its recursive metadata rejected a successful publication. This changes CI verification without changing application behavior or public APIs.
