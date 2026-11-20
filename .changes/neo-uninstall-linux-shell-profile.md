---
group: platform
component: CLI
impact: compatible
category: Fixed
---

## Summary

On Linux OverlayFS, `neo uninstall` can now remove its PATH entry from a shell profile when Linux reports an empty list of extended attributes. Profiles with visible extended attributes still require manual attention.
