# Derived route-total correction

The first report at4abd28e labelled realization-only edit totals as edit + core execution. Raw observation logs are unchanged. This revision corrects the derived report/analysis by adding the core execution stage to each edit route before taking the median.

Correct Text-edit-plus-core medians: Cabal209.198s; unsplit Nix262.835s; extracted pilot302.583s. Realization-only Nix values remain259.269s/298.890s. The pilot still fails the predeclared improvement threshold.

The raw tar retains the original derived report for audit history; use the adjacent corrected analysis.json/report.md for summaries. Current project-package configuration logs confirm GHC9.8.4/O1, split sections disabled and HIE enabled. Full equality of all dependency/compiler/linker defaults is unconfirmed; the old report assertion that current project flags differ was not supported.
