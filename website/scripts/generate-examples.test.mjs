import { test } from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { archive, exampleArchives } from './generate-examples.mjs';

test('standard tar extracts text and binary files without path or byte changes', () => {
  const temporary = mkdtempSync(join(tmpdir(), 'neo-examples-'));
  try {
    const entries = [['src/Shop/Cart/Entity.hs', Buffer.from('module Shop.Cart.Entity where\n')], ['tests/bytes.bin', Buffer.from([0, 255, 127])]];
    const path = join(temporary, 'checkpoint.tar.gz');
    writeFileSync(path, archive(entries));
    execFileSync('tar', ['-xzf', path, '-C', temporary]);
    for (const [name, contents] of entries) assert.deepEqual(readFileSync(join(temporary, name)), contents);
  } finally { rmSync(temporary, { recursive: true, force: true }); }
});

test('archive bytes do not depend on input order', () => {
  const entries = [['src/B.hs', Buffer.from('b')], ['src/A.hs', Buffer.from('a')]];
  assert.deepEqual(archive(entries), archive(entries.toReversed()));
});

test('unsafe, duplicate and unsupported paths are rejected', () => {
  for (const name of ['../outside', '/absolute', 'src/../../outside', 'src//file', 'src/./file', 'x'.repeat(101)]) {
    assert.throws(() => archive([[name, Buffer.from('x')]]), /Invalid/);
  }
  assert.throws(() => archive([['same', Buffer.from('a')], ['same', Buffer.from('b')]]), /duplicate/);
});

test('checkpoints contain application files without replacing generated project settings', () => {
  for (const example of exampleArchives) {
    const names = example.files().map(([name]) => name);
    assert.ok(names.some(name => name.startsWith('src/')));
    assert.ok(names.every(name => /^(src\/|tests\/|[A-Z]+\.md$)/.test(name)));
    assert.ok(!names.some(name => /neo\.json|\.cabal$|flake\.lock|cabal\.project/.test(name)));
  }
  const cart = exampleArchives.find(example => example.name === 'mug-shop-cart').files();
  assert.ok(cart.some(([name]) => name.endsWith('AddItemSpec.hs')));
  assert.ok(!cart.some(([name]) => name.includes('/Stock/') || name === 'src/Shop/Config.hs'));
});
