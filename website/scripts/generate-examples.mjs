#!/usr/bin/env node
// Deterministic overlays: generated project settings always remain the reader's.
import { readFileSync, readdirSync, mkdirSync, writeFileSync, existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'node:zlib';

const website = resolve(dirname(fileURLToPath(import.meta.url)), '..');

function filesUnder(directory, prefix = '') {
  return readdirSync(directory, { withFileTypes: true }).flatMap(entry => {
    const name = prefix + entry.name;
    return entry.isDirectory()
      ? filesUnder(resolve(directory, entry.name), name + '/')
      : [[name, readFileSync(resolve(directory, entry.name))]];
  });
}

function tree(root, part) {
  return filesUnder(resolve(website, 'examples', root, part), part + '/');
}

const instructions = `# Application checkpoint

These files belong in the project you created with neo new mug-shop. Extract
them into that project root, keeping the src/ and tests/ paths. They do not
replace neo.json, your launcher, or your generated project settings.

Make a commit or save your own changes before applying a checkpoint. Later
checkpoints replace earlier versions of the same feature files; merge changes
you made independently. Remove the generated Counter files and tests as shown
in the first-cart lesson. Continue using neo build, neo run, and neo test.

These examples require the upcoming framework release exporting the five derive
helpers through Core, including deriveEntity, and the corrected Neo compiler
preset described in setup. The published 0.10.0 framework pin lacks those helpers;
a corrected CLI alone is insufficient.

Lessons: https://neohaskell.org/build/first-cart/
`;

export const exampleArchives = [
  { name: 'mug-shop-first-cart', files: () => [...tree('mug-shop/first-cart', 'src'), ...tree('mug-shop/first-cart', 'tests')] },
  { name: 'mug-shop-cart', files: () => [
    ...tree('mug-shop', 'src').filter(([name]) => name.startsWith('src/Shop/Cart/')),
    ['src/App.hs', readFileSync(resolve(website, 'examples/mug-shop/first-cart/src/App.hs'))],
    ...tree('mug-shop', 'tests').filter(([name]) => !name.includes('/Stock/') && !name.endsWith('/stock-flow.hurl')),
  ] },
  { name: 'mug-shop-build', files: () => [...tree('mug-shop', 'src'), ...tree('mug-shop', 'tests')] },
  { name: 'mug-shop-connect', files: () => [
    ...tree('mug-shop-connect', 'src'), ...tree('mug-shop-connect', 'tests'),
    ['CONNECT.md', readFileSync(resolve(website, 'examples/mug-shop-connect/README.md'))],
  ] },
  { name: 'mug-shop-persistence', files: () => [
    ...tree('mug-shop/persistence', 'src'),
    ['PERSISTENCE.md', readFileSync(resolve(website, 'examples/mug-shop/persistence/README.md'))],
  ] },
];

export function archive(entries) {
  const chunks = [];
  const names = new Set();
  for (const [name, contents] of [...entries].sort(([a], [b]) => a < b ? -1 : a > b ? 1 : 0)) {
    if (!/^[a-zA-Z0-9_./-]+$/.test(name) || name.startsWith('/') || name.split('/').some(part => !part || part === '..' || part === '.') || Buffer.byteLength(name) > 100 || names.has(name)) {
      throw new Error(`Invalid or duplicate archive path: ${name}`);
    }
    names.add(name);
    const data = Buffer.from(contents);
    const header = Buffer.alloc(512);
    const field = (value, offset, size) => header.write(value, offset, size, 'ascii');
    const octal = (value, offset, size) => field(value.toString(8).padStart(size - 1, '0') + '\0', offset, size);
    field(name, 0, 100);
    octal(0o644, 100, 8);
    octal(0, 108, 8);
    octal(0, 116, 8);
    octal(data.length, 124, 12);
    octal(0, 136, 12);
    field('        ', 148, 8);
    field('0', 156, 1);
    field('ustar\0', 257, 6);
    field('00', 263, 2);
    const checksum = header.reduce((sum, byte) => sum + byte, 0);
    field(checksum.toString(8).padStart(6, '0') + '\0 ', 148, 8);
    chunks.push(header, data, Buffer.alloc((512 - data.length % 512) % 512));
  }
  chunks.push(Buffer.alloc(1024));
  return gzipSync(Buffer.concat(chunks), { level: 9 });
}

export function generateExamples(check = false) {
  const errors = [];
  for (const checkpoint of exampleArchives) {
    const path = resolve(website, 'public/examples', checkpoint.name + '.tar.gz');
    const expected = archive([...checkpoint.files(), ['CHECKPOINT.md', Buffer.from(instructions)]]);
    if (check) {
      if (!existsSync(path) || !readFileSync(path).equals(expected)) errors.push(`Stale example download: ${checkpoint.name}; run pnpm generate:examples`);
    } else {
      mkdirSync(dirname(path), { recursive: true });
      writeFileSync(path, expected);
    }
  }
  return errors;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const errors = generateExamples(process.argv.includes('--check'));
  if (errors.length) { console.error(errors.join('\n')); process.exitCode = 1; }
  else console.log(`examples: ${exampleArchives.length} deterministic checkpoints ${process.argv.includes('--check') ? 'verified' : 'generated'}`);
}
