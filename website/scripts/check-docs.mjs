#!/usr/bin/env node
// Human documentation maintenance gate. Semantic review and reader trials remain human evidence.
import { readFileSync, existsSync, readdirSync, statSync } from 'node:fs';
import { resolve, dirname, relative, posix } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import assert from 'node:assert/strict';
import { exampleArchives, generateExamples } from './generate-examples.mjs';

const website = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const repository = resolve(website, '..');
const read = path => readFileSync(path, 'utf8');
const digest = value => createHash('sha256').update(value).digest('hex');
const requiredReview = ['accessibleOpening', 'progressiveDepth', 'concreteOutcome', 'sourceGrounded', 'independence', 'domainTransfer'];
const documentationRoot = '/docs/';
const routeOf = path => '/' + path.replace(/^src\/content\/docs\//, '').replace(/\.(md|mdx)$/, '').replace(/(^|\/)index$/, '') .replace(/\/$/, '') + '/';
const normalizeRoute = route => route.replace(/\/+/g, '/');
const withoutCode = body => body.replace(/^```[^\n]*\n[\s\S]*?^```\s*$/gm, '');

// The application journey runs in the reader's project; framework contribution is a branch.
export function validateProjectWorkflow(path, body) {
  if (path.endsWith('/operate/contributing.md')) return [];
  const errors = [];
  const blocks = [...body.matchAll(/^```(?:sh|bash|shell|haskell)[^\n]*\n([\s\S]*?)^```/gm)];
  for (const [, code] of blocks) {
    if (/(?:\.\/dev\s|\bcabal\s+(?:build|run|test)\b|\bcd\s+[^\n]*\btestbed\b|git\s+clone\s+[^\n]*neohaskell\/neohaskell|\bTestbed\.)/i.test(code)) {
      errors.push(`Framework workflow in application guide: ${path}`);
    }
  }
  return errors;
}

export function validateCanonicalMarkers(path, code) {
  const errors = [];
  if (/\b(?:[A-Z]\w*\.)?(?:event|command|outboundIntegration)\s+''[A-Z]/.test(code)) errors.push(`Legacy derivation marker in tutorial: ${path}`);
  if (/^\s*import\s+Service\.(?:Event|CommandExecutor|Entity|Query|OutboundIntegration)\.TH\b/m.test(code)) errors.push(`Derivation helper must come from Core: ${path}`);
  if (/\bderiveEntity\s+''/.test(code) && /^\s*(?:instance\s+(?:(?:Json\.)?(?:FromJSON|ToJSON)|Default|Entity|Event)\b|type instance\s+(?:NameOf|EventOf|EntityOf)\b)/m.test(code)) errors.push(`Entity marker-owned boilerplate in tutorial: ${path}`);
  return errors;
}

export function validateExamplePresentation(path, body) {
  const errors = [];
  for (const match of body.matchAll(/^```haskell[^\n]*\n([\s\S]*?)^```/gm)) {
    const code = match[1];
    const prefix = body.slice(0, match.index);
    const completeFile = /<!-- complete-file -->\s*$/.test(prefix);
    if (completeFile && !/^```haskell\s+title="(?:src|tests|launcher)\/(?:[A-Za-z0-9_-]+\/)*[A-Za-z0-9_-]+\.hs"\s*\n/.test(match[0])) errors.push(`Complete file needs a project-relative destination title: ${path}`);
    if (completeFile && !/^module\s+[A-Z]/m.test(code)) errors.push(`Complete file needs its module declaration: ${path}`);
    errors.push(...validateCanonicalMarkers(path, code));
    if (/\{\-#\s*LANGUAGE\b/.test(code)) errors.push(`Language pragma in application example: ${path}`);
    if (!completeFile && /^\s*module\s+[A-Z]/m.test(code)) errors.push(`Module boilerplate in teaching example: ${path}`);
    if (!completeFile && /^\s*import\s+(?!Core(?:\s|$)|Shop\.)/m.test(code)) errors.push(`Library import scaffolding in teaching example: ${path}`);
    if (!completeFile && /\bderiving\s*(?:\(|stock\b|newtype\b|anyclass\b)/.test(code)) errors.push(`Deriving boilerplate in teaching example: ${path}`);
    const depth = [...prefix.matchAll(/<details(?:\s[^>]*)?>/g)].length - [...prefix.matchAll(/<\/details>/g)].length;
    if (!completeFile && code.trim().split('\n').length > 30 && depth <= 0) errors.push(`Long example needs progressive disclosure: ${path}`);
  }
  return errors;
}

// The first runnable lesson must be followable without downloading its checkpoint.
export function validateCompleteCheckpoint(body, entries) {
  const blocks = [...body.matchAll(/<!-- complete-file -->\s*\n```haskell title="([^"]+)"\s*\n([\s\S]*?)^```/gm)];
  const errors = [];
  for (const [path, contents] of entries) {
    if (!path.startsWith('src/') || !path.endsWith('.hs')) continue;
    const matches = blocks.filter(block => block[1] === path);
    if (matches.length !== 1) errors.push(`First slice needs one complete file for ${path}`);
    else if (matches[0][2].trim() !== contents.toString('utf8').trim()) errors.push(`First slice complete file differs from checkpoint: ${path}`);
  }
  return errors;
}

export function validate(manifest, files, sources, assets = {}) {
  const errors = [];
  const diagramFiles = new Set();
  for (const diagram of manifest.diagrams ?? []) {
    if (diagram.reviewed !== true) errors.push(`Missing diagram review: ${diagram.source}`);
    for (const [field, extension] of [['source', 'drawio'], ['export', 'svg']]) {
      const path = diagram[field];
      if (!new RegExp(`^public/diagrams/[a-z0-9-]+\\.${extension}$`).test(path ?? '')) {
        errors.push(`Invalid diagram path: ${path}`);
        continue;
      }
      const route = path.replace(/^public/, '');
      diagramFiles.add(route);
      if (!assets[route]) errors.push(`Missing diagram asset: ${path}`);
      else if (digest(assets[route]) !== diagram[`${field}Hash`]) errors.push(`Diagram changed; re-export and review: ${path}`);
    }
  }
  const screenshotFiles = new Set();
  for (const screenshot of manifest.screenshots ?? []) {
    const path = screenshot.path;
    if (screenshot.reviewed !== true) errors.push(`Missing screenshot review: ${path}`);
    if (!/^public\/screenshots\/[a-z0-9-]+\.png$/.test(path ?? '')) {
      errors.push(`Invalid screenshot path: ${path}`);
      continue;
    }
    const route = path.replace(/^public/, '');
    screenshotFiles.add(route);
    if (!assets[route]) errors.push(`Missing screenshot asset: ${path}`);
    else if (digest(assets[route]) !== screenshot.hash) errors.push(`Screenshot changed; review capture: ${path}`);
  }
  const paths = new Set();
  const routes = new Set(Object.keys(files).map(path => normalizeRoute(routeOf(path))));
  const covered = new Set();
  const edges = new Map();
  for (const path of manifest.requiredPages ?? []) {
    if (!manifest.pages.some(page => page.path === path)) errors.push(`Missing planned page: ${path}`);
  }
  for (const page of manifest.pages ?? []) {
    if (paths.has(page.path)) errors.push(`Duplicate page: ${page.path}`);
    paths.add(page.path);
    const body = files[page.path];
    if (!body) { errors.push(`Missing content: ${page.path}`); continue; }
    errors.push(...validateProjectWorkflow(page.path, body));
    errors.push(...validateExamplePresentation(page.path, body));
    if (!/^---\n[\s\S]*?^title: .+\n[\s\S]*?^description: .+\n[\s\S]*?^---/m.test(body)) errors.push(`Missing title/description: ${page.path}`);
    for (const field of requiredReview) {
      if (page.review?.[field] !== true) errors.push(`Missing editorial review ${field}: ${page.path}`);
    }
    if (!page.notes?.trim()) errors.push(`Missing review limits: ${page.path}`);
    if (!page.sources?.length) errors.push(`Missing public sources: ${page.path}`);
    for (const source of page.sources ?? []) {
      if (!sources[source]) errors.push(`Missing public source: ${source}`);
      else if (manifest.sourceHashes?.[source] !== digest(sources[source])) errors.push(`Source changed; review affected pages: ${source}`);
      if (source.startsWith('website/examples/') && source.endsWith('.hs') && /\{\-#\s*LANGUAGE\b/.test(sources[source] ?? '')) errors.push(`Language pragma in tutorial source: ${source}`);
      if (source.startsWith('website/examples/') && source.endsWith('.hs')) errors.push(...validateCanonicalMarkers(source, sources[source] ?? ''));
    }
    for (const topic of page.topics ?? []) covered.add(topic);
    const outgoing = [];
    const prose = withoutCode(body);
    const imagePattern = /!\[([^\]]*)\]\((\/[^)\s]*)(?:\s+"[^"]*")?\)/g;
    const imageMatches = [...prose.matchAll(imagePattern)];
    for (const match of imageMatches) {
      if (!match[1].trim()) errors.push(`Missing image alt text: ${page.path}`);
      if (!assets[match[2]]) errors.push(`Missing image asset ${match[2]} in ${page.path}`);
    }
    // Replacing embedded images exposes their enclosing full-size link as ordinary Markdown.
    const linkMatches = [...prose.replace(imagePattern, 'image').matchAll(/\[[^\]]*\]\((\/[^)\s]*)(?:\s+"[^"]*")?\)/g)];
    const targets = [...imageMatches.map(match => match[2]), ...linkMatches.map(match => match[1])];
    for (const target of targets) {
      if (target.startsWith('/diagrams/')) {
        if (!diagramFiles.has(target)) errors.push(`Unreviewed diagram: ${target}`);
        continue;
      }
      if (target.startsWith('/screenshots/')) {
        if (!screenshotFiles.has(target)) errors.push(`Unreviewed screenshot: ${target}`);
        continue;
      }
      if (target.startsWith('/examples/')) {
        if (!exampleArchives.some(example => target === `/examples/${example.name}.tar.gz`)) errors.push(`Unknown example download: ${target}`);
        continue;
      }
      const route = normalizeRoute(target.split(/[?#]/)[0].replace(/\/$/, '') + '/');
      outgoing.push(route);
      if (!routes.has(route)) errors.push(`Broken internal link ${target} in ${page.path}`);
    }
    edges.set(normalizeRoute(routeOf(page.path)), outgoing);
    if (/\b(?:TODO|TBD|lorem ipsum)\b/i.test(withoutCode(body))) errors.push(`Unfinished prose: ${page.path}`);
    for (const snippet of page.excerpts ?? []) {
      if (!sources[snippet.source]?.includes(snippet.text)) errors.push(`Source excerpt drift: ${page.path} (${snippet.source})`);
      if (!body.includes(snippet.text)) errors.push(`Page excerpt drift: ${page.path} (${snippet.source})`);
    }
  }
  for (const topic of manifest.requiredTopics ?? []) if (!covered.has(topic)) errors.push(`Uncovered capability: ${topic}`);
  for (const path of Object.keys(files)) {
    if (!paths.has(path)) errors.push(`Unreviewed human page: ${path}`);
  }
  const reached = new Set([documentationRoot]);
  const queue = [documentationRoot];
  while (queue.length) {
    for (const target of edges.get(queue.shift()) ?? []) {
      if (!reached.has(target)) { reached.add(target); queue.push(target); }
    }
  }
  for (const route of edges.keys()) if (!reached.has(route)) errors.push(`Unreachable from home: ${route}`);
  return [...new Set(errors)];
}

export function validateImageZoom(html) {
  const errors = [];
  const images = [...html.matchAll(/<img\b[^>]*\bsrc="(\/(?:diagrams|screenshots)\/[^"?#]+)"[^>]*>/g)];
  const zoomables = [...html.matchAll(/<starlight-image-zoom-zoomable\b[^>]*>([\s\S]*?)<\/starlight-image-zoom-zoomable>/g)];
  if (images.length && !/<starlight-image-zoom[\s>]/.test(html)) errors.push('Missing image zoom dialog controller');
  for (const [, source] of images) {
    if (!zoomables.some(([, content]) => content.includes(`src="${source}"`) && /<button\b[^>]*aria-label="Zoom image/.test(content))) {
      errors.push(`Image lacks an accessible zoom trigger: ${source}`);
    }
  }
  return errors;
}

const localRenderedTargetPattern = /(?:href|src)="(\/[^"?#]*)(?:\?[^"#]*)?(?:#([^"]*))?"/g;

// The marketing page is outside the human-doc inventory, but its local links
// and assets still need the same built-site verification as documentation.
export function validateLandingBuilt(html, targetExists) {
  const errors = [];
  if (!html) return ['Missing built marketing landing: /'];
  if (!/<main\b/.test(html) || !/<h1\b/.test(html)) errors.push('Root route is not a marketing landing page');
  if (/\bdata-has-(?:sidebar|toc)\b/.test(html) || /<nav\b[^>]*\bclass="[^"]*\bsidebar\b/.test(html)) {
    errors.push('Root route still renders documentation shell');
  }
  if (!/<a\b[^>]*\bhref="\/docs\/(?:[?#"])/.test(html)) errors.push(`Marketing landing missing docs CTA: ${documentationRoot}`);
  if (!targetExists(documentationRoot)) errors.push(`Missing built documentation root: ${documentationRoot}`);
  for (const match of html.matchAll(localRenderedTargetPattern)) {
    const target = match[1];
    if (!targetExists(target)) errors.push(`Broken landing local target ${target}`);
  }
  return [...new Set(errors)];
}

function collect(directory, prefix = '') {
  const result = {};
  for (const entry of readdirSync(directory, { withFileTypes: true })) {
    const name = posix.join(prefix, entry.name);
    if (!prefix && ['adrs', 'es', 'fr', 'hy', 'ja', 'ru'].includes(entry.name)) continue;
    if (entry.isDirectory()) Object.assign(result, collect(resolve(directory, entry.name), name));
    else if (/\.mdx?$/.test(name)) result[`src/content/docs/${name}`] = read(resolve(directory, entry.name));
  }
  return result;
}

function selfTest() {
  const path = 'src/content/docs/docs/index.md';
  const source = 'core/example.hs';
  const body = '---\ntitle: Example\ndescription: A shop\n---\nA meaningful example.\n```haskell\nexample = 1\n```\n';
  const page = { path, topics: ['commands'], sources: [source], notes: 'Source review; live execution separate.', review: Object.fromEntries(requiredReview.map(k => [k, true])), excerpts: [{ source, text: 'example = 1' }] };
  const manifest = { pages: [page], requiredPages: [path], requiredTopics: ['commands'], sourceHashes: { [source]: digest('example = 1') } };
  const files = { [path]: body };
  const sources = { [source]: 'example = 1' };
  assert.equal(routeOf('src/content/docs/docs/index.mdx'), '/docs/');
  assert.equal(routeOf('src/content/docs/start/index.md'), '/start/');
  assert.deepEqual(validate(manifest, files, sources), []);
  assert.match(validate({ ...manifest, requiredPages: [path, 'missing.md'] }, files, sources).join('\n'), /Missing planned/);
  assert.match(validate(manifest, {}, sources).join('\n'), /Missing content/);
  assert.match(validate(manifest, files, { [source]: 'example = 2' }).join('\n'), /Source changed/);
  assert.match(validate(manifest, { [path]: body + '\n[Broken](/missing/)\n' }, sources).join('\n'), /Broken internal/);
  assert.match(validate({ ...manifest, pages: [{ ...page, review: {} }] }, files, sources).join('\n'), /Missing editorial/);
  const { domainTransfer, ...previousReview } = page.review;
  assert.match(validate({ ...manifest, pages: [{ ...page, review: previousReview }] }, files, sources).join('\n'), /Missing editorial review domainTransfer/);
  assert.match(validate({ ...manifest, requiredTopics: ['queries'] }, files, sources).join('\n'), /Uncovered capability/);
  assert.match(validate(manifest, files, {}).join('\n'), /Missing public source/);
  assert.match(validate(manifest, { ...files, 'src/content/docs/new.md': body }, sources).join('\n'), /Unreviewed human page/);
  assert.match(validate(manifest, { [path]: body.replace('example = 1', 'example = 2') }, sources).join('\n'), /Page excerpt drift/);
  const orphan = { ...page, path: 'src/content/docs/orphan.md' };
  assert.match(validate({ ...manifest, pages: [page, orphan] }, { ...files, [orphan.path]: body }, sources).join('\n'), /Unreachable from home/);
  assert.deepEqual(validate(manifest, { [path]: body + '\n[Docs](/docs/)\n```text\n[Not a link](/absent/)\n```\n' }, sources), []);
  const diagram = { source: 'public/diagrams/example.drawio', export: 'public/diagrams/example.svg', sourceHash: digest('<mxfile/>'), exportHash: digest('<svg/>'), reviewed: true };
  const diagramManifest = { ...manifest, diagrams: [diagram] };
  const assets = { '/diagrams/example.drawio': '<mxfile/>', '/diagrams/example.svg': '<svg/>' };
  const illustrated = { [path]: body + '\n[![A request becomes a fact](/diagrams/example.svg)](/diagrams/example.svg "Open diagram at full size")\n' };
  assert.deepEqual(validate(diagramManifest, illustrated, sources, assets), []);
  assert.match(validate(diagramManifest, illustrated, sources, {}).join('\n'), /Missing diagram asset/);
  assert.match(validate(diagramManifest, illustrated, sources, { ...assets, '/diagrams/example.drawio': '<mxfile changed="true"/>' }).join('\n'), /Diagram changed/);
  assert.match(validate(diagramManifest, illustrated, sources, { ...assets, '/diagrams/example.svg': '<svg changed="true"/>' }).join('\n'), /Diagram changed/);
  assert.match(validate(diagramManifest, { [path]: body + '\n![](/diagrams/example.svg)\n' }, sources, assets).join('\n'), /Missing image alt text/);
  assert.match(validate(manifest, illustrated, sources, assets).join('\n'), /Unreviewed diagram/);
  assert.match(validate({ ...manifest, diagrams: [{ ...diagram, reviewed: false }] }, illustrated, sources, assets).join('\n'), /Missing diagram review/);
  assert.match(validate({ ...manifest, diagrams: [{ ...diagram, source: '../private.drawio' }] }, files, sources, assets).join('\n'), /Invalid diagram path/);
  // Non-UTF8 bytes ensure screenshot hashes describe the binary capture, not decoded text.
  const png = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0xff]);
  const screenshot = { path: 'public/screenshots/neo-ide-overview.png', hash: digest(png), reviewed: true };
  const screenshotManifest = { ...manifest, screenshots: [screenshot] };
  const screenshotAssets = { '/screenshots/neo-ide-overview.png': png };
  const captured = { [path]: body + '\n[![Commands and events in the Neo IDE](/screenshots/neo-ide-overview.png)](/screenshots/neo-ide-overview.png "Open screenshot at full size")\n' };
  assert.deepEqual(validate(screenshotManifest, captured, sources, screenshotAssets), []);
  assert.match(validate(screenshotManifest, captured, sources, {}).join('\n'), /Missing screenshot asset/);
  assert.match(validate(screenshotManifest, captured, sources, { '/screenshots/neo-ide-overview.png': png.toString('utf8') }).join('\n'), /Screenshot changed/);
  assert.match(validate(screenshotManifest, { [path]: body + '\n![ ](/screenshots/neo-ide-overview.png)\n' }, sources, screenshotAssets).join('\n'), /Missing image alt text/);
  assert.match(validate(manifest, captured, sources, screenshotAssets).join('\n'), /Unreviewed screenshot/);
  assert.match(validate({ ...manifest, screenshots: [{ ...screenshot, reviewed: false }] }, captured, sources, screenshotAssets).join('\n'), /Missing screenshot review/);
  assert.match(validate({ ...manifest, screenshots: [{ ...screenshot, path: '../private.png' }] }, files, sources, screenshotAssets).join('\n'), /Invalid screenshot path/);
  assert.match(validate({ ...manifest, screenshots: [{ ...screenshot, hash: undefined }] }, captured, sources, screenshotAssets).join('\n'), /Screenshot changed/);
  assert.deepEqual(validate(screenshotManifest, { [path]: body + '\n[Open full-size screenshot](/screenshots/neo-ide-overview.png)\n' }, sources, screenshotAssets), []);
  assert.match(validate(screenshotManifest, { [path]: body + '\n[![The IDE graph](/screenshots/neo-ide-overview.png)](/screenshots/unregistered-full-size.png)\n' }, sources, screenshotAssets).join('\n'), /Unreviewed screenshot: \/screenshots\/unregistered-full-size.png/);
  const zoomImage = '<img src="/diagrams/example.svg" alt="A request becomes a fact">';
  const zoomButton = '<button aria-label="Zoom image: A request becomes a fact"></button>';
  const zoomable = `<starlight-image-zoom-zoomable>${zoomImage}${zoomButton}</starlight-image-zoom-zoomable>`;
  const controller = '<starlight-image-zoom></starlight-image-zoom>';
  assert.deepEqual(validateImageZoom(controller + zoomable), []);
  assert.match(validateImageZoom(zoomable).join('\n'), /Missing image zoom dialog/);
  assert.match(validateImageZoom(controller + `<a href="/diagrams/example.svg">${zoomImage}</a>`).join('\n'), /lacks an accessible zoom trigger/);
  assert.match(validateImageZoom(controller + zoomable.replace(zoomButton, '')).join('\n'), /lacks an accessible zoom trigger/);
  assert.deepEqual(validateImageZoom('<p>A page with no images.</p>'), []);
  const appPage = 'src/content/docs/build/first-cart.md';
  const shellBlock = code => '```sh\n' + code + '\n```';
  assert.deepEqual(validateProjectWorkflow(appPage, shellBlock('neo new mug-shop\nneo build\nneo test')), []);
  assert.match(validateProjectWorkflow(appPage, shellBlock('git clone https://github.com/neohaskell/NeoHaskell.git')).join(''), /Framework workflow/);
  assert.match(validateProjectWorkflow(appPage, shellBlock('cd NeoHaskell/testbed')).join(''), /Framework workflow/);
  assert.match(validateProjectWorkflow(appPage, shellBlock('./dev test')).join(''), /Framework workflow/);
  assert.match(validateProjectWorkflow(appPage, shellBlock('cabal run nhtestbed')).join(''), /Framework workflow/);
  assert.deepEqual(validateProjectWorkflow('src/content/docs/operate/contributing.md', shellBlock('./dev test')), []);
  assert.deepEqual(validateProjectWorkflow(appPage, '[Source](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)'), []);
  const smallExample = '```haskell\nimport Core\n\nquantity = 2\n```';
  const longExample = '```haskell\n' + Array.from({ length: 31 }, (_, i) => `field${i} = ${i}`).join('\n') + '\n```';
  assert.deepEqual(validateExamplePresentation(appPage, smallExample), []);
  assert.match(validateExamplePresentation(appPage, longExample).join(''), /progressive disclosure/);
  assert.deepEqual(validateExamplePresentation(appPage, '<details><summary>Complete file</summary>\n' + longExample + '\n</details>'), []);
  assert.match(validateExamplePresentation(appPage, '<details><summary>Earlier</summary></details>\n' + longExample).join(''), /progressive disclosure/);
  assert.match(validateExamplePresentation(appPage, '```haskell\n{-# LANGUAGE TemplateHaskell #-}\n```').join(''), /Language pragma/);
  assert.match(validateExamplePresentation(appPage, '<details><summary>Complete file</summary>\n```haskell\n{-# LANGUAGE TemplateHaskell #-}\n```\n</details>').join(''), /Language pragma/);
  const tutorialSource = 'website/examples/example.hs';
  const sourceCheck = (sourcePath, contents) => validate(
    { ...manifest, sourceHashes: { [sourcePath]: digest(contents) }, pages: [{ ...page, sources: [sourcePath], excerpts: [] }] },
    files, { [sourcePath]: contents },
  );
  assert.deepEqual(sourceCheck(tutorialSource, 'module Example where\nimport Core\n'), []);
  assert.match(sourceCheck(tutorialSource, '{-# LANGUAGE TemplateHaskell #-}\nimport Core\n').join(''), /Language pragma in tutorial source/);
  assert.deepEqual(sourceCheck('core/Example.hs', '{-# LANGUAGE TemplateHaskell #-}\nimport Core\n'), []);
  assert.match(validateExamplePresentation(appPage, '```haskell\nmodule Shop.Cart where\n```').join(''), /Module boilerplate/);
  assert.match(validateExamplePresentation(appPage, '```haskell\nimport Array qualified\n```').join(''), /Library import scaffolding/);
  assert.deepEqual(validateExamplePresentation(appPage, '```haskell\nimport Core\nimport Shop.Cart qualified\n```'), []);
  assert.deepEqual(validate(manifest, { [path]: body + '\n[Checkpoint](/examples/mug-shop-first-cart.tar.gz)\n' }, sources), []);
  assert.match(validate(manifest, { [path]: body + '\n[Missing](/examples/absent.tar.gz)\n' }, sources).join(''), /Unknown example download/);
  assert.match(validateExamplePresentation(appPage, '```haskell\ndata Event = Event deriving (Show)\n```').join(''), /Deriving boilerplate/);
  assert.match(validateExamplePresentation(appPage, '```haskell\nderiving stock instance Show Event\n```').join(''), /Deriving boilerplate/);
  assert.deepEqual(validateExamplePresentation(appPage, "```haskell\ndata Event = Event\n\nderiveEvent ''Event\n```"), []);
  for (const marker of ['deriveEvent', 'deriveCommand', 'deriveEntity', 'deriveQuery', 'deriveOutboundIntegration']) {
    assert.deepEqual(validateCanonicalMarkers(appPage, `import Core\n${marker} ''Example`), []);
  }
  for (const marker of ['event', 'EventTH.event', 'command', 'outboundIntegration']) {
    assert.match(validateCanonicalMarkers(appPage, `${marker} ''Example`).join(''), /Legacy derivation marker/);
  }
  assert.deepEqual(validateCanonicalMarkers(appPage, 'event = acceptedFact\nservice |> Service.command @CreateCart'), []);
  assert.match(validateCanonicalMarkers(appPage, 'import Service.Query.TH (deriveQuery)').join(''), /must come from Core/);
  assert.match(validateCanonicalMarkers(appPage, "instance Default CartEntity where\n  def = initialState\nderiveEntity ''CartEntity ''CartEvent").join(''), /marker-owned boilerplate/);
  assert.match(sourceCheck(tutorialSource, "import Core\ncommand ''Example").join(''), /Legacy derivation marker/);
  const completeFile = code => '<!-- complete-file -->\n```haskell title="src/Shop/Cart/Entity.hs"\n' + code + '\n```';
  const completeSource = 'module Shop.Cart.Entity where\nimport Core\nimport Uuid qualified\n' + Array.from({ length: 31 }, (_, i) => `field${i} = ${i}`).join('\n');
  assert.deepEqual(validateExamplePresentation(appPage, completeFile(completeSource)), []);
  assert.match(validateExamplePresentation(appPage, completeFile(completeSource).replace(' title="src/Shop/Cart/Entity.hs"', '')).join(''), /destination title/);
  assert.match(validateExamplePresentation(appPage, completeFile(completeSource).replace('src/Shop/Cart/Entity.hs', '../Entity.hs')).join(''), /destination title/);
  assert.match(validateExamplePresentation(appPage, completeFile('import Core')).join(''), /module declaration/);
  assert.match(validateExamplePresentation(appPage, completeFile('{-# LANGUAGE TemplateHaskell #-}\n' + completeSource)).join(''), /Language pragma/);
  assert.match(validateExamplePresentation(appPage, completeFile(completeSource + "\ncommand ''Example")).join(''), /Legacy derivation marker/);
  assert.match(validateExamplePresentation(appPage, completeFile(completeSource + "\nimport Service.Event.TH")).join(''), /must come from Core/);
  assert.match(validateExamplePresentation(appPage, completeFile(completeSource + "\ninstance Default CartEntity where\n  def = initialState\nderiveEntity ''CartEntity ''CartEvent")).join(''), /marker-owned boilerplate/);
  assert.match(validateExamplePresentation(appPage, '<!-- complete-file -->\nUnrelated explanation\n' + longExample).join(''), /progressive disclosure/);
  const checkpoint = [['src/Shop/Cart/Entity.hs', Buffer.from(completeSource)]];
  assert.deepEqual(validateCompleteCheckpoint(completeFile(completeSource), checkpoint), []);
  assert.match(validateCompleteCheckpoint('', checkpoint).join(''), /needs one complete file/);
  assert.match(validateCompleteCheckpoint(completeFile(completeSource + '\nextra = 1'), checkpoint).join(''), /differs from checkpoint/);
  assert.match(validateCompleteCheckpoint(completeFile(completeSource).repeat(2), checkpoint).join(''), /needs one complete file/);
  assert.deepEqual(validateCompleteCheckpoint('', [['tests/scenarios/create-cart.hurl', Buffer.from('GET /')]]), []);
  const landingTargets = new Set(['/', '/docs/', '/start/', '/favicon.svg']);
  const landing = '<html><body><main><h1>NeoHaskell</h1><a href="/docs/">Read the docs</a><img src="/favicon.svg" alt=""></main></body></html>';
  const hasLandingTarget = target => landingTargets.has(target);
  assert.deepEqual(validateLandingBuilt(landing, hasLandingTarget), []);
  assert.match(validateLandingBuilt('', hasLandingTarget).join(''), /Missing built marketing landing/);
  assert.match(validateLandingBuilt(landing, target => target !== '/docs/' && hasLandingTarget(target)).join(''), /Missing built documentation root/);
  assert.match(validateLandingBuilt(landing.replace('/docs/', '/missing/'), hasLandingTarget).join(''), /Broken landing local target/);
  assert.match(validateLandingBuilt(landing.replace('/favicon.svg', '/missing.svg'), hasLandingTarget).join(''), /Broken landing local target/);
  assert.match(validateLandingBuilt(landing.replace('<a href="/docs/">Read the docs</a>', '<a href="/start/">Start</a>'), hasLandingTarget).join(''), /missing docs CTA/);
  assert.match(validateLandingBuilt('<html data-has-sidebar><body><main><h1>NeoHaskell</h1></main></body></html>', hasLandingTarget).join(''), /documentation shell/);
  console.log('docs-check: 96 positive, negative, and boundary assertions passed');
}

function checkBuilt(manifest) {
  const failures = [];
  for (const page of manifest.pages) {
    const route = normalizeRoute(routeOf(page.path));
    const output = resolve(website, 'dist', route.slice(1), 'index.html');
    if (!existsSync(output)) { failures.push(`Missing built route: ${route}`); continue; }
    const html = read(output);
    for (const error of validateImageZoom(html)) failures.push(`${error} on ${route}`);
    for (const match of html.matchAll(/(?:href|src)="(\/[^"?#]*)(?:\?[^"#]*)?(?:#([^"]*))?"/g)) {
      const target = resolve(website, 'dist', decodeURIComponent(match[1]).slice(1));
      const targetFile = existsSync(target) && statSync(target).isDirectory() ? resolve(target, 'index.html') : target;
      if (!existsSync(targetFile)) { failures.push(`Broken rendered link ${match[1]} on ${route}`); continue; }
      if (match[2] && targetFile.endsWith('.html')) {
        const anchor = decodeURIComponent(match[2]);
        if (!read(targetFile).includes(`id="${anchor}"`)) failures.push(`Missing anchor #${anchor} at ${match[1]} from ${route}`);
      }
    }
  }
  const landingOutput = resolve(website, 'dist', 'index.html');
  const targetExists = target => {
    const targetPath = resolve(website, 'dist', decodeURIComponent(target).slice(1));
    const targetFile = existsSync(targetPath) && statSync(targetPath).isDirectory() ? resolve(targetPath, 'index.html') : targetPath;
    return existsSync(targetFile);
  };
  const landingHtml = existsSync(landingOutput) ? read(landingOutput) : '';
  failures.push(...validateLandingBuilt(landingHtml, targetExists));
  if (failures.length) throw new Error([...new Set(failures)].join('\n'));
  console.log(`docs-check: ${manifest.pages.length} built pages and their local links/anchors passed`);
}

if (process.argv.includes('--self-test')) selfTest();
else {
  try {
    const manifest = JSON.parse(read(resolve(website, 'documentation-manifest.json')));
    const files = collect(resolve(website, 'src/content/docs'));
    const sources = {};
    for (const path of Object.keys(manifest.sourceHashes ?? {})) {
      const absolute = resolve(repository, path);
      if (relative(repository, absolute).startsWith('..') || path.startsWith('/')) throw new Error(`Source must be repository-local: ${path}`);
      if (existsSync(absolute)) sources[path] = read(absolute);
    }
    const assets = {};
    for (const diagram of manifest.diagrams ?? []) {
      for (const field of ['source', 'export']) {
        const path = diagram[field];
        if (typeof path === 'string' && /^public\/diagrams\/[a-z0-9-]+\.(drawio|svg)$/.test(path)) {
          const absolute = resolve(website, path);
          if (existsSync(absolute)) assets[path.replace(/^public/, '')] = read(absolute);
        }
      }
    }
    for (const screenshot of manifest.screenshots ?? []) {
      const path = screenshot.path;
      if (typeof path === 'string' && /^public\/screenshots\/[a-z0-9-]+\.png$/.test(path)) {
        const absolute = resolve(website, path);
        if (existsSync(absolute)) assets[path.replace(/^public/, '')] = readFileSync(absolute);
      }
    }
    const errors = validate(manifest, files, sources, assets);
    errors.push(...validateCompleteCheckpoint(files['src/content/docs/build/first-cart.md'] ?? '', exampleArchives.find(example => example.name === 'mug-shop-first-cart').files()));
    errors.push(...generateExamples(true));
    for (const checkpoint of exampleArchives) {
      for (const [path, contents] of checkpoint.files()) {
        if (path.endsWith('.hs')) errors.push(...validateCanonicalMarkers(`${checkpoint.name}/${path}`, contents.toString('utf8')));
      }
    }
    for (const document of ['DOCUMENTATION_PLAN.md', 'DOCUMENTATION_REVIEW.md']) {
      if (!existsSync(resolve(website, document))) errors.push(`Missing methodology artifact: ${document}`);
    }
    if (errors.length) throw new Error(errors.join('\n'));
    console.log(`docs-check: ${manifest.pages.length} reviewed pages; ${manifest.requiredTopics.length} required capabilities covered`);
    if (process.argv.includes('--built')) checkBuilt(manifest);
  } catch (error) { console.error(`docs-check: ${error.message}`); process.exitCode = 1; }
}
