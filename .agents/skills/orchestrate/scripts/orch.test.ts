import { afterEach, describe, it } from "node:test";
import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import { chmod, mkdir, mkdtemp, readFile, readdir, rename, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { openStore, UserError, NotFoundError, type Store, type RecordLedgerParams } from "./store.ts";

const roots: string[] = [], handles: Store[] = [];
const originalPath = process.env.PATH;
const originalFixture = process.env.ORCH_FIXTURE;
const repositoryRoot = execFileSync("git", ["rev-parse", "--show-toplevel"], { encoding: "utf8" }).trim();
const H = (n: number) => n.toString(16).repeat(40);
const script = new URL("./orch.ts", import.meta.url).pathname;
async function directory() {
  const parent = join(repositoryRoot, ".agents-state", "orchestrate-tests");
  await mkdir(parent, { recursive: true });
  const path = await mkdtemp(join(parent, "case-")); roots.push(path);
  return path;
}
async function initialized() {
  const path = await directory(); const store = openStore(path); handles.push(store);
  await store.init(); await writeFile(join(path, "proof.txt"), "fixture verification receipt\n");
  return { path, store };
}
async function reopen(path: string) { const store = openStore(path); handles.push(store); return store; }
function proof(overrides: Partial<RecordLedgerParams> = {}): RecordLedgerParams {
  return { repo: "test/framework", pr: 3, sha: H(3), base: H(2), inputs: H(4), claim: "behavior",
    verdict: "behavior-verified", author: "writer", verifier: "reviewer", independent: true,
    evidence: "proof.txt", method: "execute real fixture and assert result", ...overrides };
}
const check = (repo: string, extra = {}) => ({ repo, pr: 3, sha: H(3), base: H(2), inputs: H(4), claim: "behavior" as const, ...extra });
type Pr = { number: number; state: string; headRefName: string; baseRefName: string; headRefOid: string; baseRefOid: string };
async function forge(path: string) {
  const repo = join(path, "repo"), bin = join(path, "bin");
  await mkdir(repo); await mkdir(bin);
  await writeFile(join(bin, "package.json"), '{"type":"commonjs"}\n');
  const data = { repo: { nameWithOwner: "test/framework", defaultBranchRef: { name: "trunk" } }, current: "work",
    prs: [
      { number: 1, state: "MERGED", headRefName: "parent", baseRefName: "trunk", headRefOid: H(1), baseRefOid: H(0) },
      { number: 2, state: "CLOSED", headRefName: "closed", baseRefName: "parent", headRefOid: H(2), baseRefOid: H(1) },
      { number: 3, state: "OPEN", headRefName: "work", baseRefName: "closed", headRefOid: H(3), baseRefOid: H(2) },
    ] as Pr[] };
  const file = join(path, "fixture.json");
  const save = async () => writeFile(file, JSON.stringify(data)); await save();
  await writeFile(join(bin, "gh"), `#!${process.execPath}
const fs=require('node:fs');const x=JSON.parse(fs.readFileSync(process.env.ORCH_FIXTURE));const a=process.argv.slice(2);
if(a[0]==='repo'&&a[1]==='view') console.log(JSON.stringify(x.repo));
else if(a[0]==='pr'&&a[1]==='view') console.log(JSON.stringify(x.prs.find(p=>p.number===Number(a[2]))));
else if(a[0]==='pr'&&a[1]==='list') console.log(JSON.stringify(x.prs.filter(p=>p[a[2]==='--head'?'headRefName':'baseRefName']===a[3])));
else {console.error('unexpected gh args',a);process.exit(2);}
`);
  await writeFile(join(bin, "git"), `#!${process.execPath}
const fs=require('node:fs');const a=process.argv.slice(2);if(a.join(' ')==='symbolic-ref --short HEAD')console.log(JSON.parse(fs.readFileSync(process.env.ORCH_FIXTURE)).current);else{console.error('forbidden local-ref git call',a);process.exit(2);}
`);
  await chmod(join(bin, "gh"), 0o755); await chmod(join(bin, "git"), 0o755);
  process.env.PATH = `${bin}:${originalPath}`; process.env.ORCH_FIXTURE = file;
  return { repo, data, save };
}
function cli(args: string[], env = process.env) {
  return spawnSync(process.execPath, [script, ...args], { env, encoding: "utf8" });
}
afterEach(async () => {
  for (const h of handles.splice(0).reverse()) await h.close();
  process.env.PATH = originalPath;
  if (originalFixture === undefined) delete process.env.ORCH_FIXTURE; else process.env.ORCH_FIXTURE = originalFixture;
  for (const root of roots.splice(0)) await rm(root, { recursive: true, force: true });
});

describe("plain-file state", () => {
  it("initializes idempotently, updates scoped units, renders gates/status and releases its lock", async () => {
    const { path, store } = await initialized(); await store.init();
    await store.units.add({ id: "u", track: "compiler" });
    await store.units.set({ id: "u", state: "needs-verify", branch: "work", pr: 3, sha: H(3) });
    assert.equal((await store.units.get("u")).state, "needs-verify");
    await assert.rejects(store.units.add({ id: "u", track: "duplicate" }), UserError);
    await store.gates.park({ id: "land", question: "land?", options: "yes/no", defaultAnswer: "hold" });
    await store.standing.add({ line: "Do not merge without authorization." });
    const result = await store.status.render(); assert.equal(result.summary.openGateIds.length, 1);
    assert.match(await readFile(join(path, "status.md"), "utf8"), /needs-verify/);
    await store.gates.resolve({ id: "land", answer: "yes" }); assert.deepEqual(await store.gates.list(), []);
    await store.close(); assert.ok(!(await readdir(path)).includes(".orch.lock"));
  });
  it("rejects a second live writer and reopens after close", async () => {
    const { path, store } = await initialized(); const second = await reopen(path);
    await assert.rejects(second.units.add({ id: "other", track: "x" }), /lock held/);
    await store.close(); await second.units.add({ id: "other", track: "x" });
  });
  it("rejects legacy ledger headers instead of guessing a migration", async () => {
    const { path, store } = await initialized(); await writeFile(join(path, "ledger.tsv"), "pr\tsha\tverdict\tevidence\tverifier\tts\n");
    await assert.rejects(store.ledger.summary(), /invalid header/);
  });
});

describe("current-input independent evidence", () => {
  it("accepts matching independent behavior proof and retains provenance", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await store.ledger.record(proof()); const result = await store.ledger.check(check(f.repo));
    assert.equal(result.base, H(2)); assert.equal(result.verifier, "reviewer");
  });
  it("rejects author self-attestation as independence", async () => {
    const { store } = await initialized(); await assert.rejects(store.ledger.record(proof({ verifier: "WRITER" })), /distinct verifier/);
  });
  it("does not let a later worker self-report replace independent proof", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await store.ledger.record(proof());
    await store.ledger.record(proof({ independent: false, verifier: "writer", verdict: "verifier-failed" }));
    assert.equal((await store.ledger.check(check(f.repo))).verifier, "reviewer");
    assert.equal((await readFile(join(path, "ledger.tsv"), "utf8")).trim().split("\n").length, 3);
  });
  it("worker-only evidence cannot satisfy landing verification", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await store.ledger.record(proof({ independent: false, verifier: "writer" }));
    await assert.rejects(store.ledger.check(check(f.repo)), NotFoundError);
  });
  for (const kind of ["verifier-failed", "verifier-blocked"] as const) {
    it(`blocks ${kind} and permits a fresh pass after it, even with the same receipt path`, async () => {
      const { path, store } = await initialized(); const f = await forge(path);
      await store.ledger.record(proof()); await store.ledger.record(proof({ verdict: kind }));
      await assert.rejects(store.ledger.check(check(f.repo)), new RegExp(kind));
      await store.ledger.record(proof()); assert.equal((await store.ledger.check(check(f.repo))).verdict, "behavior-verified");
      const count = (await readFile(join(path, "ledger.tsv"), "utf8")).split("\n").length;
      await store.ledger.record(proof()); assert.equal((await readFile(join(path, "ledger.tsv"), "utf8")).split("\n").length, count);
    });
  }
  it("accepts a fresh pass after another independent verifier's failure without deduping it into history", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await store.ledger.record(proof()); await store.ledger.record(proof({verifier:"second-reviewer",verdict:"verifier-failed"}));
    await assert.rejects(store.ledger.check(check(f.repo)),/verifier-failed/);
    await store.ledger.record(proof()); assert.equal((await store.ledger.check(check(f.repo))).verifier,"reviewer");
  });
  it("rejects stale remote head and base even when local/recorded identity is unchanged", async () => {
    const { path, store } = await initialized(); const f = await forge(path); await store.ledger.record(proof());
    f.data.prs[2]!.headRefOid = H(7); await f.save(); await assert.rejects(store.ledger.check(check(f.repo)), /head\/base changed/);
    f.data.prs[2]!.headRefOid = H(3); f.data.prs[2]!.baseRefOid = H(8); await f.save();
    await assert.rejects(store.ledger.check(check(f.repo)), /head\/base changed/);
  });
  it("rejects stale input digests and a different repository", async () => {
    const { path, store } = await initialized(); const f = await forge(path); await store.ledger.record(proof());
    await assert.rejects(store.ledger.check(check(f.repo, { inputs: H(5) })), NotFoundError);
    f.data.repo.nameWithOwner = "other/framework"; await f.save(); await assert.rejects(store.ledger.check(check(f.repo)), NotFoundError);
  });
  it("accepts compiler-only proof for a compiler contract but not a behavioral claim", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await store.ledger.record(proof({ claim: "compiler", verdict: "compiler-verified" }));
    assert.equal((await store.ledger.check(check(f.repo, { claim: "compiler" }))).verdict, "compiler-verified");
    await assert.rejects(store.ledger.check(check(f.repo)), NotFoundError);
    await assert.rejects(store.ledger.record(proof({ verdict: "compiler-verified" })), /declared claim/);
  });
  it("requires an extant receipt and an open candidate", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await assert.rejects(store.ledger.record(proof({ evidence: "absent.txt" })), /receipt is missing/);
    await store.ledger.record(proof()); f.data.prs[2]!.state = "MERGED"; await f.save();
    await assert.rejects(store.ledger.check(check(f.repo)), /PR state/);
    f.data.prs[2]!.state = "OPEN"; await f.save(); await rm(join(path, "proof.txt"));
    await assert.rejects(store.ledger.check(check(f.repo)), /receipt is missing/);
  });
});

describe("GitHub frontier", () => {
  it("uses published head/base identities without local refs, and keeps a closed parent as the blocker", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    const result = await store.frontier.set({ repo: f.repo });
    assert.equal(result.repo, "test/framework"); assert.equal(result.trunk, "trunk");
    assert.deepEqual(result.prs.map(p => [p.pr, p.sha, p.baseSha]), [[1,H(1),H(0)],[2,H(2),H(1)],[3,H(3),H(2)]]);
    assert.equal(result.lowestUnmerged, 2);
    f.data.prs[2]!.headRefOid = H(6); await f.save();
    assert.equal((await store.frontier.set({ repo: f.repo })).prs[2]!.sha, H(6));
  });
  it("finds a closed prerequisite from below while excluding an abandoned historical sibling", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    f.data.current = "parent";
    f.data.prs.push({ number: 9, state: "CLOSED", headRefName: "abandoned", baseRefName: "parent", headRefOid: H(9), baseRefOid: H(1) });
    await f.save(); const result = await store.frontier.set({ repo: f.repo });
    assert.deepEqual(result.prs.map(p => p.pr), [1,2,3]); assert.equal(result.lowestUnmerged, 2);
  });
  it("fails on unknown/default-branch/missing/ambiguous observations without advancing the saved generation", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await store.frontier.set({ repo: f.repo });
    for (const mutate of [() => f.data.prs[1]!.state = "UNKNOWN", () => f.data.repo.defaultBranchRef.name = "", () => f.data.prs.pop(), () => f.data.prs.push({...f.data.prs[2]!,number:9})]) {
      const original = structuredClone(f.data); mutate(); await f.save();
      await assert.rejects(store.frontier.set({ repo: f.repo })); assert.equal((await store.frontier.show()).generation, 1);
      Object.assign(f.data, original); await f.save();
    }
  });
  it("rejects non-linear chains, parent/head drift and incorrect order pins", async () => {
    const { path, store } = await initialized(); const f = await forge(path);
    await assert.rejects(store.frontier.set({ repo: f.repo, prs: [3,2,1] }), /order/);
    f.data.prs[2]!.baseRefOid = H(7); await f.save(); await assert.rejects(store.frontier.set({ repo: f.repo }), /drifted/);
    f.data.prs[2]!.baseRefOid = H(2); f.data.prs.push({ number: 4, state: "OPEN", headRefName: "sibling", baseRefName: "closed", headRefOid: H(4), baseRefOid: H(2) });
    f.data.current = "closed"; await f.save(); await assert.rejects(store.frontier.set({ repo: f.repo }), /not linear/);
  });
});

describe("recoverable completion batches", () => {
  it("recovers the exact unacknowledged batch after process/store restart and retains new arrivals", async () => {
    const { path, store } = await initialized(); await store.units.add({ id:"u",track:"x" });
    await store.inbox.push({agent:"worker",unit:"u",status:"done"}); const first = await store.inbox.drain();
    await store.units.set({ id:"u",state:"needs-verify" }); await store.close();
    const resumed = await reopen(path); const again = await resumed.inbox.drain(); assert.deepEqual(again, first);
    await resumed.inbox.push({agent:"next",unit:"v",status:"done"}); assert.equal(await resumed.inbox.count(),2);
    await writeFile(join(path,"receipt.json"),JSON.stringify({batch:first.id,entries:[{pointer:first.pointers[0]!.id,action:"unit",state:"needs-verify"}]}));
    assert.equal((await resumed.inbox.ack(first.id!,"receipt.json")).count,1);
    assert.equal((await resumed.inbox.ack(first.id!,"receipt.json")).count,1);
    const next=await resumed.inbox.drain(); assert.notEqual(next.id,first.id); assert.equal(next.pointers[0]!.unit,"v");
    assert.ok((await readdir(join(path,"accounted",first.id!))).includes("accounting.json"));
  });
  it("recovers a crash between moving the inbox and recreating the arrivals directory", async () => {
    const { path, store } = await initialized(); await store.inbox.push({agent:"w",unit:"u",status:"done"});
    await mkdir(join(path,"batches")); const id="12345678-1234-1234-1234-123456789012";
    await rename(join(path,"inbox"),join(path,"batches",id)); await store.close();
    const resumed=await reopen(path); assert.equal((await resumed.inbox.drain()).id,id);
    await resumed.inbox.push({agent:"new",unit:"v",status:"done"}); assert.equal(await resumed.inbox.count(),2);
  });
  it("acknowledges verifier results only after the unit and matching ledger row are saved", async () => {
    const { path, store } = await initialized(); await store.units.add({id:"u",track:"x"});
    await store.inbox.push({agent:"v",unit:"u",status:"verified"}); const batch=await store.inbox.drain();
    await store.ledger.record(proof());
    const file=join(path,"receipt.json");
    const entry={pointer:batch.pointers[0]!.id,action:"ledger",repo:"test/framework",pr:3,sha:H(3),base:H(2),inputs:H(4),claim:"behavior",verifier:"reviewer"};
    await writeFile(file,JSON.stringify({batch:batch.id,entries:[entry]}));
    await assert.rejects(store.inbox.ack(batch.id!,file),/not saved/);
    await store.units.set({id:"u",state:"verified",pr:3,sha:H(3)});
    assert.equal((await store.inbox.ack(batch.id!,file)).count,1);
  });
  it("rejects duplicate pointer accounting even when the entry count matches", async () => {
    const { path, store } = await initialized();
    await store.inbox.push({agent:"w",unit:"u",status:"done"}); await store.inbox.push({agent:"w",unit:"v",status:"done"});
    const batch=await store.inbox.drain(); const entry={pointer:batch.pointers[0]!.id,action:"noise",reason:"duplicate"};
    await writeFile(join(path,"receipt.json"),JSON.stringify({batch:batch.id,entries:[entry,entry]}));
    await assert.rejects(store.inbox.ack(batch.id!,"receipt.json"),/duplicate/);
    assert.equal((await store.inbox.drain()).pointers.length,2);
  });
  it("refuses partial, duplicate or unsaved accounting and keeps the same pending batch", async () => {
    const { path, store } = await initialized(); await store.units.add({id:"u",track:"x"});
    await store.inbox.push({agent:"w",unit:"u",status:"done"}); const batch=await store.inbox.drain();
    const file=join(path,"receipt.json");
    for(const entries of [[],[{pointer:batch.pointers[0]!.id,action:"unit",state:"not-saved"}], [{pointer:"unknown",action:"noise",reason:"x"}]]) {
      await writeFile(file,JSON.stringify({batch:batch.id,entries})); await assert.rejects(store.inbox.ack(batch.id!,file));
      assert.deepEqual(await store.inbox.drain(),batch);
    }
    await writeFile(file,JSON.stringify({batch:batch.id,entries:[{pointer:batch.pointers[0]!.id,action:"noise",reason:"duplicate notification; original already recorded"}]}));
    await store.inbox.ack(batch.id!,file); assert.deepEqual(await store.inbox.drain(),{id:null,pointers:[]});
  });
});

describe("CLI", () => {
  it("prints help without installing dependencies or mutating a store", async () => {
    const path=await directory(); const result=cli(["--help"],{...process.env,ORCH_STORE:join(path,"absent")});
    assert.equal(result.status,0,result.stderr); assert.match(result.stdout,/inbox/); assert.deepEqual(await readdir(path),[]);
  });
  it("records/checks proof and drains/acknowledges a saved completion through real CLI argument parsing", async () => {
    const { path, store }=await initialized(); const f=await forge(path); await store.close();
    const prefix=["--store",path,"--json"];
    const record=cli([...prefix,"ledger","record","3",H(3),"behavior-verified","--repo","test/framework","--base",H(2),"--inputs",H(4),"--claim","behavior","--method","execute fixture","--evidence","proof.txt","--author","writer","--verifier","reviewer","--independent"]);
    assert.equal(record.status,0,record.stderr); assert.equal(JSON.parse(record.stdout).independent,true);
    const checked=cli([...prefix,"ledger","check","3",H(3),"--repo",f.repo,"--base",H(2),"--inputs",H(4),"--claim","behavior"]);
    assert.equal(checked.status,0,checked.stderr); assert.equal(JSON.parse(checked.stdout).verdict,"behavior-verified");
    assert.equal(cli([...prefix,"unit","add","u","--track","x"]).status,0);
    assert.equal(cli([...prefix,"unit","set","u","--state","needs-verify"]).status,0);
    assert.equal(cli([...prefix,"inbox","push","w","u","done"]).status,0);
    const drained=cli([...prefix,"inbox","drain"]); assert.equal(drained.status,0,drained.stderr); const batch=JSON.parse(drained.stdout);
    await writeFile(join(path,"receipt.json"),JSON.stringify({batch:batch.id,entries:[{pointer:batch.pointers[0].id,action:"unit",state:"needs-verify"}]}));
    const ack=cli([...prefix,"inbox","ack",batch.id,"--receipt","receipt.json"]); assert.equal(ack.status,0,ack.stderr);
    assert.equal(JSON.parse(ack.stdout).count,1);
  });
  it("requires explicit proof context and exposes recoverable drain/ack", async () => {
    const path=await directory(); assert.equal(cli(["--store",path,"init"]).status,0);
    const missing=cli(["--store",path,"ledger","record","3",H(3),"behavior-verified","--evidence","proof.txt"]);
    assert.equal(missing.status,1); assert.match(missing.stderr,/required option/);
    const output=cli(["--store",path,"--json","inbox","drain"]); assert.equal(output.status,0,output.stderr);
    assert.deepEqual(JSON.parse(output.stdout),{id:null,pointers:[]});
    assert.match(cli(["inbox","ack","--help"]).stdout,/receipt/);
    assert.match(cli(["frontier","set","--help"]).stdout,/gh/);
  });
});
