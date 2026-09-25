#!/usr/bin/env python3
import json
from pathlib import Path
import sys
for filename in sys.argv[1:]:
    source=Path(filename)
    graph=json.loads(source.read_text())
    root=(source.parent/'root.txt').read_text().strip()
    def size(roots):
        seen=set()
        todo=list(roots)
        while todo:
            path=todo.pop()
            if path in seen:
                continue
            seen.add(path)
            todo.extend(graph[path]['references'])
        return {'paths':len(seen),'nar_bytes':sum(graph[path]['narSize'] for path in seen)}
    runtime=[path for path in graph if path.endswith('-neohaskell-ci-runtime')]
    assert len(runtime)==1
    executables=[path for path in graph if '-nhcore-test-nhcore-test-' in path or '-nhintegrations-test-nhintegrations-test-' in path or '-nhtestbed-exe-nhtestbed-' in path]
    assert len(executables)==6
    print(json.dumps({'source':str(source),'full':size([root]),'components':[{'root':path,'executable_closure':size([path]),'with_existing_runtime':size([path]+runtime)} for path in sorted(executables)]},indent=2))
