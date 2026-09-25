# Per-test closure analysis by root

Computed from publisher36163339478 closure.json dependency graphs, archived at352dbff. Traverse references transitively with deduplication and sum narSize. These are unpacked NAR bytes, not compressed transfer bytes or latency. No new builds, omitted references, or production topology changes.

Linux full bundle:659 paths /6019.3MiB. Every test executable plus existing shared runtime:87 paths /380.6–397.4MiB. Testbed:387.3MiB. Much of the full bundle consists of development/library outputs that these executable closures do not need. This supports measuring narrower transfer while retaining parallel test execution.

macOS full bundle:646 paths /5591.6MiB. nhintegrations executable plus runtime:78 paths /736.6MiB; core/service/auth/integration suites:206–207 paths /3996.0–4040.5MiB. Unlike Linux, most macOS test binaries retain a large transitive closure. Do not project Linux transfer savings onto macOS or prune live references.

Decision: first verify the corrected signed-public-cache fetch. Then measure a narrow executable+existing-runtime bundle against the full bundle with identical source/cache conditions. Keep current fanout, required suite inventory, fixtures, codemap/doctest, reports and failure gates. Do not reduce runtime packages yet: retaining the working runtime isolates the effect of excluding unrelated components. Measure producer encoding/upload costs and consumer download/import separately, three observations per decision. Only change production after actual correctness and critical-path/aggregate measurements; byte reduction alone is not a latency result.

Reproduce by extracting archived publisher metadata from352dbff and running replay.py on each published-component-metadata-*/closure.json. Its root is the adjacent root.txt. Machine-readable observations name every exact selected executable output.
