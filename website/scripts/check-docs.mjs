#!/usr/bin/env node
// Human documentation maintenance gate. Semantic review and reader trials remain human evidence.
import { readFileSync, existsSync, readdirSync, statSync } from 'node:fs';
import { resolve, dirname, relative, posix } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import assert from 'node:assert/strict';

const website = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const repository = resolve(website, '..');
const read = path => readFileSync(path, 'utf8');
const digest = value => createHash('sha256').update(value).digest('hex');
const requiredReview = ['accessibleOpening', 'progressiveDepth', 'concreteOutcome', 'sourceGrounded', 'independence', 'domainTransfer'];
const routeOf = path => '/' + path.replace(/^src\/content\/docs\//, '').replace(/\.(md|mdx)$/, '').replace(/(^|\/)index$/, '') .replace(/\/$/, '') + '/';
const normalizeRoute = route => route.replace(/\/+/g, '/');
const withoutCode = body => body.replace(/^```[^\n]*\n[\s\S]*?^```\s*$/gm, '');

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
    if (!/^---\n[\s\S]*?^title: .+\n[\s\S]*?^description: .+\n[\s\S]*?^---/m.test(body)) errors.push(`Missing title/description: ${page.path}`);
    for (const field of requiredReview) {
      if (page.review?.[field] !== true) errors.push(`Missing editorial review ${field}: ${page.path}`);
    }
    if (!page.notes?.trim()) errors.push(`Missing review limits: ${page.path}`);
    if (!page.sources?.length) errors.push(`Missing public sources: ${page.path}`);
    for (const source of page.sources ?? []) {
      if (!sources[source]) errors.push(`Missing public source: ${source}`);
      else if (manifest.sourceHashes?.[source] !== digest(sources[source])) errors.push(`Source changed; review affected pages: ${source}`);
    }
    for (const topic of page.topics ?? []) covered.add(topic);
    const outgoing = [];
    for (const match of withoutCode(body).matchAll(/!\[([^\]]*)\]\((\/[^)\s]*)(?:\s+"[^"]*")?\)/g)) {
      if (!match[1].trim()) errors.push(`Missing image alt text: ${page.path}`);
      if (!assets[match[2]]) errors.push(`Missing image asset ${match[2]} in ${page.path}`);
    }
    for (const match of withoutCode(body).matchAll(/\[[^\]]*\]\((\/[^)\s]*)(?:\s+"[^"]*")?\)/g)) {
      if (match[1].startsWith('/diagrams/')) {
        if (!diagramFiles.has(match[1])) errors.push(`Unreviewed diagram: ${match[1]}`);
        continue;
      }
      const route = normalizeRoute(match[1].split(/[?#]/)[0].replace(/\/$/, '') + '/');
      outgoing.push(route);
      if (!routes.has(route)) errors.push(`Broken internal link ${match[1]} in ${page.path}`);
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
  const reached = new Set(['/']);
  const queue = ['/'];
  while (queue.length) {
    for (const target of edges.get(queue.shift()) ?? []) {
      if (!reached.has(target)) { reached.add(target); queue.push(target); }
    }
  }
  for (const route of edges.keys()) if (!reached.has(route)) errors.push(`Unreachable from home: ${route}`);
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
  const path = 'src/content/docs/index.md';
  const source = 'core/example.hs';
  const body = '---\ntitle: Example\ndescription: A shop\n---\nA meaningful example.\n```haskell\nexample = 1\n```\n';
  const page = { path, topics: ['commands'], sources: [source], notes: 'Source review; live execution separate.', review: Object.fromEntries(requiredReview.map(k => [k, true])), excerpts: [{ source, text: 'example = 1' }] };
  const manifest = { pages: [page], requiredPages: [path], requiredTopics: ['commands'], sourceHashes: { [source]: digest('example = 1') } };
  const files = { [path]: body };
  const sources = { [source]: 'example = 1' };
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
  assert.deepEqual(validate(manifest, { [path]: body + '\n[Home](/)\n```text\n[Not a link](/absent/)\n```\n' }, sources), []);
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
  console.log('docs-check: 21 positive, negative, and boundary cases passed');
}

function checkBuilt(manifest) {
  const failures = [];
  for (const page of manifest.pages) {
    const route = normalizeRoute(routeOf(page.path));
    const output = resolve(website, 'dist', route.slice(1), 'index.html');
    if (!existsSync(output)) { failures.push(`Missing built route: ${route}`); continue; }
    const html = read(output);
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
    const errors = validate(manifest, files, sources, assets);
    for (const document of ['DOCUMENTATION_PLAN.md', 'DOCUMENTATION_REVIEW.md']) {
      if (!existsSync(resolve(website, document))) errors.push(`Missing methodology artifact: ${document}`);
    }
    if (errors.length) throw new Error(errors.join('\n'));
    console.log(`docs-check: ${manifest.pages.length} reviewed pages; ${manifest.requiredTopics.length} required capabilities covered`);
    if (process.argv.includes('--built')) checkBuilt(manifest);
  } catch (error) { console.error(`docs-check: ${error.message}`); process.exitCode = 1; }
}
