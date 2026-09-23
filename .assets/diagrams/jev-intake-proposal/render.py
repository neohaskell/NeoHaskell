from pathlib import Path
import subprocess,tempfile,json,concurrent.futures,re
OUT=Path(__file__).parent
chrome='/Users/nick/Library/Caches/ms-playwright/chromium_headless_shell-1243/chrome-headless-shell-mac-arm64/chrome-headless-shell'
mxgraph=Path('/Users/nick/.codex/artifacts/neohaskell-pipeline/mxgraph').as_uri()
pages=[('01-flow',2010),('02-task-types',1390),('03-decisions',1360)]
def render(item):
 name,height=item
 xml=json.dumps((OUT/(name+'.drawio')).read_text())
 html='''<!doctype html><html><head><meta charset="utf-8"><style>html,body{margin:0;background:#fff}#graph{width:1600px;height:HEIGHTpx}</style><script>var mxBasePath='MXGRAPH';var mxLoadResources=false;var mxLoadStylesheets=false;</script><script src="MXGRAPH/mxClient.js"></script></head><body><div id="graph"></div><script>var graph=new mxGraph(document.getElementById('graph'));graph.setHtmlLabels(true);graph.setEnabled(false);var doc=mxUtils.parseXml(XML);new mxCodec(doc).decode(doc.getElementsByTagName('mxGraphModel')[0],graph.getModel());document.body.setAttribute('data-rendered','true');</script></body></html>'''.replace('HEIGHT',str(height)).replace('MXGRAPH',mxgraph).replace('XML',xml)
 page=OUT/(name+'.html');page.write_text(html)
 cmd=[chrome,'--disable-gpu','--hide-scrollbars','--no-proxy-server','--user-data-dir='+tempfile.mkdtemp(prefix='intake-diagram-'),'--window-size=1600,'+str(height),'--virtual-time-budget=2000','--screenshot='+str(OUT/(name+'.png')),'--dump-dom',page.as_uri()]
 with (OUT/(name+'.dom.html')).open('w') as f:
  result=subprocess.run(cmd,stdout=f,stderr=subprocess.PIPE,text=True,timeout=25)
 assert result.returncode==0,result.stderr[-700:]
 dom=(OUT/(name+'.dom.html')).read_text()
 assert 'data-rendered="true"' in dom, name+' did not render'
 paths=re.findall(r'<path[^>]* d="([^"]+)"',dom)
 (OUT/(name+'.paths.json')).write_text(json.dumps(paths,indent=2))
 return name+': rendered; '+str(len(paths))+' SVG paths'
with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
 for result in pool.map(render,pages):print(result)
