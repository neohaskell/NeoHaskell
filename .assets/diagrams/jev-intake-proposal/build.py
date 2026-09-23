from pathlib import Path
import xml.etree.ElementTree as E
OUT=Path(__file__).parent
PALETTE={'model':('#e1d5e7','#9673a6'),'tool':('#fff2cc','#d6b656'),'action':('#dae8fc','#6c8ebf'),'stop':('#ffe6cc','#d79b00'),'output':('#d5e8d4','#82b366'),'note':('#f5f5f5','#b3bdca')}
mx=E.Element('mxfile',host='drawio',version='26.0.0');pages=[]
class Page:
 def __init__(self,id,title,subtitle,height):
  self.id=id;self.height=height;self.d=E.SubElement(mx,'diagram',id=id,name=title)
  self.g=E.SubElement(self.d,'mxGraphModel',grid='1',gridSize='10',page='1',pageWidth='1600',pageHeight=str(height),background='#ffffff')
  self.r=E.SubElement(self.g,'root');E.SubElement(self.r,'mxCell',id='0');E.SubElement(self.r,'mxCell',id='1',parent='0');self.n=0
  self.text('title',title,40,25,1520,60,34)
  self.text('sub',subtitle,40,90,1520,45,19)
  pages.append(self)
 def text(self,id,value,x,y,w,h,size=17):
  return self.vertex(id,value,x,y,w,h,f'text;html=1;whiteSpace=wrap;align=left;verticalAlign=middle;fontFamily=Helvetica;fontSize={size};fontColor=#263449;fillColor=none;strokeColor=none;')
 def vertex(self,id,value,x,y,w,h,style,source=''):
  c=E.SubElement(self.r,'mxCell',id=id,value=value,style=style,vertex='1',parent='1')
  if source:c.set('data-source',source)
  E.SubElement(c,'mxGeometry',x=str(x),y=str(y),width=str(w),height=str(h),attrib={'as':'geometry'})
  return c
 def box(self,id,title,body,x,y,w=380,h=120,role='action',source=''):
  fill,stroke=PALETTE[role]
  value=f'<b style="font-size:21px">{title}</b><div style="font-size:17px;line-height:1.35;margin-top:8px">{body}</div>'
  return self.vertex(id,value,x,y,w,h,f'rounded=1;arcSize=10;whiteSpace=wrap;html=1;spacing=18;align=left;verticalAlign=middle;fontFamily=Helvetica;fontSize=17;fontColor=#172b42;fillColor={fill};strokeColor={stroke};strokeWidth=2;',source)
 def decision(self,id,title,x,y,w=380,h=170,role='model',source=''):
  fill,stroke=PALETTE[role]
  value=f'<span style="font-size:12px">{role.upper()}</span><br><b>{title}</b>'
  return self.vertex(id,value,x,y,w,h,f'shape=rhombus;perimeter=rhombusPerimeter;whiteSpace=wrap;html=1;align=center;verticalAlign=middle;fontFamily=Helvetica;fontSize=18;fontColor=#172b42;fillColor={fill};strokeColor={stroke};strokeWidth=2;spacing=2;',source)
 def edge(self,a,b,label='',direction='down',points=None,pins=None,dashed=False):
  self.n+=1
  ps={'down':(.5,1,.5,0),'right':(1,.5,0,.5),'left':(0,.5,1,.5),'up':(.5,0,.5,1)}[direction] if pins is None else pins
  style='edgeStyle=orthogonalEdgeStyle;rounded=1;orthogonalLoop=1;jettySize=auto;html=1;endArrow=block;endSize=8;strokeWidth=2;strokeColor=#5b6f87;fontFamily=Helvetica;fontSize=16;fontColor=#263449;labelBackgroundColor=#ffffff;'
  style+=f'exitX={ps[0]};exitY={ps[1]};entryX={ps[2]};entryY={ps[3]};'
  if dashed:style+='dashed=1;'
  c=E.SubElement(self.r,'mxCell',id=f'e{self.n}',value=label,style=style,edge='1',parent='1',source=a,target=b)
  geo=E.SubElement(c,'mxGeometry',relative='1',attrib={'as':'geometry'})
  if points:
   arr=E.SubElement(geo,'Array',attrib={'as':'points'})
   for x,y in points:E.SubElement(arr,'mxPoint',x=str(x),y=str(y))
 def footer(self,source):
  y=self.height-110
  for i,(role,label) in enumerate([('model','Jev judgment'),('tool','Code decision'),('action','Code / lookup'),('stop','Human / LLM fallback'),('output','Typed handoff')]):
   f,s=PALETTE[role];x=40+i*300
   self.vertex('legend-'+role,'',x,y,20,20,f'html=1;fillColor={f};strokeColor={s};')
   self.text('legend-label-'+role,label,x+30,y-5,255,30,16)
  self.text('source',source,40,y+45,1520,45,14)

p=Page('01-flow','Proposed intake · code in control, Jev for meaning','REVIEW DRAFT · A design proposal, not implemented behavior. Zero or a small bounded number of Jev calls; LLM fallback only for a named gap.',2010)
p.box('request','Request + existing session','Keep explicit task settings and user constraints.',580,165,460,85,'output')
p.box('facts','CODE · Read facts; lock constraints','Source: issue / ad hoc / saved session.<br>Branch, worktree, run state, IDs and budget.<br>Explicit pause takes priority over new work.',580,305,460,120)
p.decision('explicit','Required intent / mode<br>already explicit?',610,475,400,150,'tool')
p.box('batch-a','JEV A · Fill unresolved intent slots','CHOICE: primary work type and execution mode.<br>NOUL: additional requested actions / mixed work.<br>Never overwrite an explicit setting.',580,700,460,125,'model')
p.box('lookup','CODE · Load the matching playbook','Retrieve only its required facts and candidates.<br>Code paths, skill IDs, checkpoints, logs or PRs.<br>Operational tasks need no code localization.',580,905,460,130)
p.box('batch-b','JEV B · Judge retrieved candidates','NOUL: does each candidate apply?<br>SCORE: evidence fit for each candidate.<br>CHOICE: known next action or missing field.',580,1110,460,130,'model')
p.decision('usable','Required fields resolved<br>and checks pass?',610,1320,400,150,'tool')
p.box('brief','CODE · Compile an intake record','Task + mode + ordered actions + targets.<br>Evidence references, stop rules and budgets.',580,1555,460,110,'output')
p.box('dispatch','CODE · Run a registered playbook','Tools handle known steps and state transitions.<br>Invoke an LLM only for a step needing<br>new code, prose, planning or diagnosis.',580,1745,460,130)
p.decision('gap','What is unresolved?',60,1320,350,150,'tool')
p.box('fetch','Missing observable evidence','Fetch the named source, once within budget.<br>Rejudge only affected questions;<br>if still unresolved, clarify or escalate.',60,1100,350,140)
p.box('ask','Missing user intent','Ask one question naming the choice.<br>Re-enter with the reply and saved state.',60,1555,350,110,'stop')
p.box('reason','Novel interpretation / plan','Give an LLM only the unresolved slice.<br>Its proposal returns through the same checks.<br>No expansion of authority.',60,1745,350,130,'stop')
p.edge('request','facts');p.edge('facts','explicit');p.edge('explicit','batch-a','No')
p.edge('explicit','lookup','Yes — skip A',points=[(1090,550),(1090,970)],pins=(1,.5,1,.5))
p.edge('batch-a','lookup');p.edge('lookup','batch-b','Semantic slots remain')
p.edge('lookup','usable','Skip B',points=[(1100,1010),(1100,1395)],pins=(1,.8,1,.5))
p.edge('batch-b','usable','Validate IDs, evidence and policy')
p.edge('usable','brief','Yes');p.edge('brief','dispatch')
p.edge('usable','gap','No','left');p.edge('gap','fetch','Evidence','up')
p.edge('fetch','lookup','New evidence',points=[(470,1170),(470,970)],pins=(1,.5,0,.5))
p.edge('gap','ask','Intent');p.edge('gap','reason','Novel',points=[(30,1395),(30,1810)],pins=(0,.5,0,.5))
p.edge('reason','usable','Revalidate',points=[(470,1810),(470,1510),(710,1510)],pins=(1,.5,.25,.75),dashed=True)
p.box('locked','A typed decision is not permission','Your “same branch / same worktree” rule<br>is a locked constraint, not a Jev judgment.<br><br>Publishing, merging or deleting still needs<br>the authority required by that action.',1150,165,400,210,'note')
p.box('batch-note','Batch independent questions','One shared state, separate narrow questions.<br>Speculative questions state their premise.<br>Code reads only the selected branch.<br><br>Reuse answers while inputs stay unchanged.<br>New evidence needed? Fetch, then call again.',1150,650,400,230,'note')
p.box('score-note','Evidence fit ≠ confidence','0 · unrelated or contradicted<br>1 · related vocabulary / dependency<br>2 · partial support; key fact missing<br>3 · directly supports this target<br><br>Rubric is proposed. Thresholds need data.',1150,1110,400,220,'note')
p.box('checks','The final check is deterministic','Known IDs and valid output schema.<br>Required facts exist and remain current.<br>Mixed tasks have a valid sequence.<br>Only relevant uncertainty is considered.<br>Constraints and action authority are intact.<br><br>Low confidence alone is not a blanket veto.',1150,1535,400,240,'note')
p.footer('Proposed from the TypeSafe skill + fan-out / intent-routing patterns. Budgeted fallback and task presets are design choices; no speed or accuracy claim is implied.')

p=Page('02-task-types','Your 22 task names · presets that compose','Keep the familiar names in the UI. Internally separate what to do, how to run, and which lifecycle actions were requested.',1390)
p.box('profile','One request → a structured profile','Primary work + optional secondary work · execution mode · ordered lifecycle actions · locked constraints',300,170,1000,100,'output')
work=[('authoring a skill','Create or revise a reusable skill.'),('bug fix','Reproduce and correct wrong behavior.'),('eval','Measure a system against reference cases.'),('feature','Add an agreed capability.'),('hillclimb','Improve a metric within fixed correctness limits.'),('investigation','Answer a question with evidence.'),('multi phase plan','Produce an ordered plan and dependencies.'),('perf issue','Investigate a measured performance problem.'),('prototype','Test feasibility with bounded scope.'),('refactoring','Change structure while preserving behavior.'),('runtime forensics','Diagnose a live or captured runtime state.'),('trace forensics','Reconstruct behavior from traces or logs.')]
modes=[('autonomous run','Complete one bounded task within a budget.'),('autopilot full','Run an agreed end-to-end playbook to its stop point.'),('autopilot stack','Coordinate dependent changes / PR layers.'),('babysit','Monitor an existing run; recover known failures.'),('orchestrate','Coordinate work items, dependencies and workers.')]
actions=[('opening a pr','Prepare or open the requested PR.'),('pause safely','Checkpoint, record running work, and stop.'),('session pickup','Validate and resume a saved task.'),('shipping','Execute the agreed release / delivery steps.'),('worktree cleanup','Inspect and clean only authorized targets.')]
def items(rows):
 return ''.join('<div style="margin-bottom:13px"><b>'+a+'</b><br><span style="font-size:15px">'+b+'</span></div>' for a,b in rows)
p.box('work','WHAT · primary work type',items(work),40,380,480,775,'model')
p.box('mode','HOW · execution mode',items(modes)+'<br><b>Mode is not authority.</b><br>“Autopilot full” still stops at the agreed<br>boundary. Default to bounded work when<br>the mode is unspecified.',560,380,480,775,'model')
p.box('actions','WHEN · lifecycle actions',items(actions)+'<br><b>Actions can accompany any work type.</b><br>They are ordered steps, not competing<br>labels. An action-only request needs<br>no invented feature or bug task.',1080,380,480,775,'model')
p.edge('profile','work','CHOICE + optional extra-intent NOULs',points=[(450,320),(280,320)],pins=(.15,1,.5,0))
p.edge('profile','mode','CHOICE; explicit setting wins')
p.edge('profile','actions','One NOUL per applicable action',points=[(1150,320),(1320,320)],pins=(.85,1,.5,0))
p.box('example','Example · “Pick up the bug fix, run autonomously, then open a PR”','Work = bug fix · Mode = autonomous run · Actions = session pickup → opening a pr.<br>Constraints = preserve this branch/worktree · Stop = the explicitly agreed PR state.',200,1200,1200,95,'output')
p.text('note','Definitions are proposals. Perf issue / runtime forensics / trace forensics can share the investigation engine while retaining distinct presets.',40,1320,1520,45,16)

p=Page('03-decisions','Decision design · use each primitive for its actual meaning','Draft question contract. Exact facts and permissions stay in code; Jev only fills semantic gaps with evidence.',1360)
p.box('choice','CHOICE · one mutually exclusive answer','<b>Primary work type?</b><br>Choose from the 12 work presets, mixed,<br>action-only, or unknown.<br><br><b>Execution mode?</b><br>Choose only if the user has not set it.<br><br><b>Which named evidence source is missing?</b><br>Pick from available source IDs, none, unknown.<br><br>Never force a match when no option fits.',40,190,480,490,'model')
p.box('noul','NOUL · independent yes/no questions','<b>Does this request also ask for a PR?</b><br><b>Does capability C own part of the change?</b><br><b>Does resolving this gap require user intent?</b><br><br>One question per independent proposition.<br>Several capabilities/actions may be true.<br><br>Returns P(yes), not intensity.<br>A value near 0.5 means uncertainty.<br>No separate confidence is returned.',560,190,480,490,'model')
p.box('score','SCORE · grade one dimension','<b>How well does evidence support target C?</b><br>0 unrelated / contradicted<br>1 indirect overlap<br>2 partial support<br>3 direct support<br><br>Compare targets on the same rubric.<br>Keep the full probability distribution.<br><br>Do not turn one blended “quality score”<br>into permission to run the workflow.',1080,190,480,490,'model')
p.box('code','CODE · decisions that need no model','Explicit commands, file/path existence, Git state,<br>request source, allowed operations, budgets,<br>schema checks, risk-tag joins, call-site expansion,<br>test selectors, exit codes, timers and receipts.<br><br>Choose templates from verified registries;<br>fill parameters with exact IDs or source spans.',40,770,480,315)
p.box('fallback','LLM · bounded work at named gaps','Novel multi-step planning or diagnosis.<br>Writing new code, tests, skill prose or explanations.<br>Interpreting evidence when Jev + code cannot.<br><br>Hand over a compact verified intake record.<br>Preserve the task and constraints; do not restart<br>the whole intake conversation.',560,770,480,315,'stop')
p.box('metrics','EVAL · prove each replacement helps','Wrong automatic routes AND automatic coverage.<br>Unnecessary clarification and missed ambiguity.<br>Intake LLM-free rate; Jev calls and tokens.<br>p50/p95 latency; cost estimates; repeat stability.<br>Evidence validity; preserved user constraints.<br><br>Separate data for policy tuning and evaluation.<br>Check whole workflows as well as questions.',1080,770,480,315,'output')
p.box('lesson','What our experiment changes about this proposal','The three prompt rewrites did not improve raw judgment. Treat this architecture as a hypothesis:<br>replace one decision at a time, compare against the current process, and keep only demonstrated improvements.',150,1150,1300,100,'note')
p.text('footer','Sources: TypeSafe skill; docs.typesafe.ai/patterns/fan-out; docs.typesafe.ai/patterns/intent-routing; local Jev comparison artifacts. Proposed behavior only.',40,1290,1520,45,15)

for page in pages:
 if page.id != '01-flow':
  for cell in page.r.findall('mxCell'):
   if cell.get('id') in {'work','mode','actions','choice','noul','score','code','fallback','metrics'}:
    cell.set('style',cell.get('style')+'verticalAlign=top;')
E.indent(mx)
E.ElementTree(mx).write(OUT/'jev-intake-proposal.drawio',encoding='utf-8',xml_declaration=True)
for p in pages:
 one=E.Element('mxfile',host='drawio',version='26.0.0');one.append(p.d)
 E.ElementTree(one).write(OUT/(p.id+'.drawio'),encoding='utf-8',xml_declaration=True)
print('Generated',len(pages),'review pages')
