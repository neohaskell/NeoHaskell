import { execFileSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import type { Dirent } from "node:fs";
import {
  access,
  mkdir,
  open,
  readFile,
  readdir,
  rename,
  rm,
  unlink,
  writeFile,
} from "node:fs/promises";
import { basename, dirname, join, resolve } from "node:path";

const UNIT_HEADER = "id\ttrack\tstate\tbranch\tpr\tsha\tbrief";
const LEDGER_HEADER = "repo\tpr\tsha\tbase\tinputs\tclaim\tverdict\tevidence\tverifier\tauthor\tmethod\tindependent\tts";
const LOCK_FILE = ".orch.lock";

export type Verdict =
  | "artifact-verified"
  | "behavior-verified"
  | "compiler-verified"
  | "verifier-blocked"
  | "verifier-failed";

export interface Unit {
  readonly id: string;
  readonly track: string;
  readonly state: string;
  readonly branch: string;
  readonly pr: string;
  readonly sha: string;
  readonly brief: string;
}

export interface LedgerEntry {
  readonly repo: string;
  readonly pr: string;
  readonly sha: string;
  readonly base: string;
  readonly inputs: string;
  readonly claim: "compiler" | "behavior" | "artifact";
  readonly verdict: Verdict;
  readonly evidence: string;
  readonly verifier: string;
  readonly author: string;
  readonly method: string;
  readonly independent: boolean;
  readonly ts: string;
}

export interface InboxPointer {
  readonly id: string;
  readonly ts: string;
  readonly agent: string;
  readonly unit: string;
  readonly status: string;
  readonly report: string;
}

export interface InboxBatch {
  readonly id: string | null;
  readonly pointers: readonly InboxPointer[];
}

export interface InboxPushResult {
  readonly pointer: InboxPointer;
  readonly filename: string;
}

export interface OpenGate {
  readonly kind: "open";
  readonly id: string;
  readonly question: string;
  readonly options: string;
  readonly defaultAnswer: string;
}

export interface ResolvedGate {
  readonly kind: "resolved";
  readonly id: string;
  readonly question: string;
  readonly options: string;
  readonly defaultAnswer: string;
  readonly answer: string;
}

export type Gate = OpenGate | ResolvedGate;

export type FrontierPrState = "OPEN" | "MERGED" | "CLOSED";

export interface FrontierPr {
  readonly pr: number;
  readonly branches: string;
  readonly sha: string;
  readonly base: string;
  readonly baseSha: string;
  readonly state: FrontierPrState;
}

export interface Frontier {
  readonly repo: string;
  readonly trunk: string;
  readonly observedAt: string;
  readonly generation: number;
  readonly prs: readonly FrontierPr[];
  readonly lowestUnmerged: number | null;
}

export interface StandingLine {
  readonly number: number;
  readonly line: string;
}

export type Counts = Readonly<Record<string, number>>;

export interface StatusSummary {
  readonly unitStates: Counts;
  readonly ledgerVerdicts: Counts;
  readonly frontierGeneration: number;
  readonly openGateIds: readonly string[];
}

export interface StatusReport {
  readonly units: readonly Unit[];
  readonly ledger: readonly LedgerEntry[];
  readonly frontier: Frontier;
  readonly gates: readonly Gate[];
  readonly summary: StatusSummary;
  readonly changed: string;
}

export interface AddUnitParams {
  readonly id: string;
  readonly track: string;
  readonly brief?: string;
}

export interface SetUnitParams {
  readonly id: string;
  readonly state: string;
  readonly branch?: string;
  readonly pr?: number;
  readonly sha?: string;
}

export interface ListUnitsParams {
  readonly state?: string;
  readonly track?: string;
}

export interface RecordLedgerParams {
  readonly repo: string;
  readonly pr: number;
  readonly sha: string;
  readonly base: string;
  readonly inputs: string;
  readonly claim: "compiler" | "behavior" | "artifact";
  readonly verdict: Verdict;
  readonly evidence: string;
  readonly verifier: string;
  readonly author: string;
  readonly method: string;
  readonly independent?: boolean;
}

export interface CheckLedgerParams {
  readonly repo: string;
  readonly pr: number;
  readonly sha: string;
  readonly base: string;
  readonly inputs: string;
  readonly claim: "compiler" | "behavior" | "artifact";
}

export interface PushInboxParams {
  readonly agent: string;
  readonly unit: string;
  readonly status: string;
  readonly report?: string;
}

export interface ParkGateParams {
  readonly id: string;
  readonly question: string;
  readonly options: string;
  readonly defaultAnswer: string;
}

export interface ResolveGateParams {
  readonly id: string;
  readonly answer: string;
}

export interface SetFrontierParams {
  readonly repo: string;
  readonly forge?: Forge;
  readonly prs?: readonly number[];
}

export interface AddStandingParams {
  readonly line: string;
}

export interface OpenStoreOptions {
  readonly force?: boolean;
  readonly onLockStolen?: (holder: string) => void;
  readonly onStaleLock?: (holder: string) => void;
}

export interface Store {
  readonly units: {
    readonly add: (params: AddUnitParams) => Promise<Unit>;
    readonly set: (params: SetUnitParams) => Promise<Unit>;
    readonly get: (id: string) => Promise<Unit>;
    readonly list: (params?: ListUnitsParams) => Promise<readonly Unit[]>;
    readonly counts: () => Promise<Counts>;
  };
  readonly ledger: {
    readonly record: (params: RecordLedgerParams) => Promise<LedgerEntry>;
    readonly check: (params: CheckLedgerParams) => Promise<LedgerEntry>;
    readonly summary: () => Promise<Counts>;
  };
  readonly inbox: {
    readonly push: (params: PushInboxParams) => Promise<InboxPushResult>;
    readonly drain: () => Promise<InboxBatch>;
    readonly ack: (id: string, receipt: string) => Promise<{ readonly id: string; readonly count: number }>;
    readonly peek: () => Promise<readonly InboxPointer[]>;
    readonly count: () => Promise<number>;
  };
  readonly gates: {
    readonly park: (params: ParkGateParams) => Promise<OpenGate>;
    readonly list: () => Promise<readonly OpenGate[]>;
    readonly resolve: (params: ResolveGateParams) => Promise<ResolvedGate>;
  };
  readonly frontier: {
    readonly set: (params: SetFrontierParams) => Promise<Frontier>;
    readonly show: () => Promise<Frontier>;
  };
  readonly standing: {
    readonly show: () => Promise<readonly StandingLine[]>;
    readonly add: (params: AddStandingParams) => Promise<StandingLine>;
  };
  readonly status: {
    readonly render: () => Promise<StatusReport>;
  };
  readonly init: () => Promise<{ readonly store: string }>;
  readonly close: () => Promise<void>;
}

export interface NotFoundOutput {
  readonly compact: string;
  readonly json: unknown;
}

export class UserError extends Error {}
export class UsageError extends UserError {}
export class NotFoundError extends UserError {
  readonly output?: NotFoundOutput;
  constructor(message: string, output?: NotFoundOutput) {
    super(message);
    this.output = output;
  }
}

function errorCode(error: unknown): string | null {
  if (
    error !== null &&
    typeof error === "object" &&
    "code" in error &&
    typeof error.code === "string"
  ) {
    return error.code;
  }
  return null;
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function isUnknownArray(value: unknown): value is readonly unknown[] {
  return Array.isArray(value);
}

function verdictOrNull(value: string): Verdict | null {
  switch (value) {
    case "artifact-verified":
    case "behavior-verified":
    case "compiler-verified":
    case "verifier-blocked":
    case "verifier-failed":
      return value;
    default:
      return null;
  }
}

function frontierPrStateOrNull(value: unknown): FrontierPrState | null {
  switch (value) {
    case "OPEN":
    case "MERGED":
    case "CLOSED":
      return value;
    default:
      return null;
  }
}

export function parseVerdict(value: string): Verdict {
  const verdict = verdictOrNull(value);
  if (verdict === null) {
    throw new UserError(
      "verdict must be artifact-verified, behavior-verified, compiler-verified, verifier-blocked, or verifier-failed"
    );
  }
  return verdict;
}

function cleanCell(value: string): string {
  const cleaned = value.replace(/[\t\n\r]/g, " ");
  return /^[=+\-@]/.test(cleaned) ? `'${cleaned}` : cleaned;
}

function requiredCell(value: string, label: string): string {
  const cleaned = cleanCell(value);
  if (cleaned.trim().length === 0) {
    throw new UserError(`${label} must not be empty`);
  }
  return cleaned;
}

function requiredLine(value: string, label: string): string {
  const cleaned = value.replace(/[\n\r]/g, " ").trim();
  if (cleaned.length === 0) {
    throw new UserError(`${label} must not be empty`);
  }
  return cleaned;
}

function positiveInteger(value: number, label: string): number {
  if (!Number.isSafeInteger(value) || value < 1) {
    throw new UserError(`${label} must be a positive integer`);
  }
  return value;
}

async function exists(path: string): Promise<boolean> {
  try {
    await access(path);
    return true;
  } catch (error) {
    if (errorCode(error) === "ENOENT") {
      return false;
    }
    throw error;
  }
}

async function atomicWrite(path: string, contents: string): Promise<void> {
  const temporary = join(
    dirname(path),
    `.${basename(path)}.${process.pid}.${randomUUID()}.tmp`
  );
  try {
    await writeFile(temporary, contents, { flag: "wx" });
    await rename(temporary, path);
  } finally {
    await rm(temporary, { force: true });
  }
}

async function writeIfMissing(path: string, contents: string): Promise<void> {
  if (!(await exists(path))) {
    await atomicWrite(path, contents);
  }
}

async function requiredFile(path: string): Promise<string> {
  try {
    return await readFile(path, "utf8");
  } catch (error) {
    if (errorCode(error) === "ENOENT") {
      throw new UserError(
        `store is not initialized at ${dirname(path)}; run orch init`
      );
    }
    throw error;
  }
}

function holderIsDead(holder: string): boolean {
  const pid = Number.parseInt(holder, 10);
  if (!Number.isSafeInteger(pid) || pid <= 0 || String(pid) !== holder) {
    return false;
  }
  try {
    process.kill(pid, 0);
    return false;
  } catch (error) {
    return errorCode(error) === "ESRCH";
  }
}

async function acquireLock(
  store: string,
  options: OpenStoreOptions
): Promise<() => Promise<void>> {
  const path = join(store, LOCK_FILE);
  const pid = String(process.pid);
  const create = async (): Promise<void> => {
    const handle = await open(path, "wx");
    await handle.writeFile(`${pid}\n`);
    await handle.close();
  };

  const takeOver = async (): Promise<void> => {
    await unlink(path);
    try {
      await create();
    } catch (retryError) {
      if (errorCode(retryError) === "EEXIST") {
        const retryHolder =
          (await readFile(path, "utf8")).trim() || "unknown";
        throw new UserError(`store lock held by pid ${retryHolder}`);
      }
      throw retryError;
    }
  };

  try {
    await create();
  } catch (error) {
    if (errorCode(error) !== "EEXIST") {
      throw error;
    }
    let holder = "unknown";
    try {
      holder = (await readFile(path, "utf8")).trim() || "unknown";
    } catch {
      holder = "unknown";
    }
    if (holderIsDead(holder)) {
      options.onStaleLock?.(holder);
      await takeOver();
    } else if (options.force) {
      options.onLockStolen?.(holder);
      await takeOver();
    } else {
      throw new UserError(`store lock held by pid ${holder}`);
    }
  }

  return async (): Promise<void> => {
    try {
      if ((await readFile(path, "utf8")).trim() === pid) {
        await unlink(path);
      }
    } catch (error) {
      if (errorCode(error) !== "ENOENT") {
        throw error;
      }
    }
  };
}

async function readTsv(
  path: string,
  header: string,
  width: number
): Promise<readonly (readonly string[])[]> {
  const lines = (await requiredFile(path)).replace(/\r/g, "").split("\n");
  if (lines.shift() !== header) {
    throw new UserError(`${basename(path)} has an invalid header`);
  }
  return lines
    .filter((value) => value.length > 0)
    .map((value) => {
      const cells = value.split("\t");
      if (cells.length !== width) {
        throw new UserError(`${basename(path)} has a malformed row`);
      }
      return cells;
    });
}

async function writeTsv(
  path: string,
  header: string,
  rows: readonly (readonly string[])[]
): Promise<void> {
  const body = rows.map((row) => row.map(cleanCell).join("\t")).join("\n");
  await atomicWrite(path, `${header}\n${body}${body.length > 0 ? "\n" : ""}`);
}

async function readUnits(store: string): Promise<readonly Unit[]> {
  return (await readTsv(join(store, "units.tsv"), UNIT_HEADER, 7)).map(
    (row) => ({
      id: row[0] ?? "",
      track: row[1] ?? "",
      state: row[2] ?? "",
      branch: row[3] ?? "",
      pr: row[4] ?? "",
      sha: row[5] ?? "",
      brief: row[6] ?? "",
    })
  );
}

function unitCells(unit: Unit): readonly string[] {
  return [
    unit.id,
    unit.track,
    unit.state,
    unit.branch,
    unit.pr,
    unit.sha,
    unit.brief,
  ];
}

async function saveUnits(store: string, rows: readonly Unit[]): Promise<void> {
  await writeTsv(
    join(store, "units.tsv"),
    UNIT_HEADER,
    rows.map(unitCells)
  );
}

async function readLedger(store: string): Promise<readonly LedgerEntry[]> {
  return (await readTsv(join(store, "ledger.tsv"), LEDGER_HEADER, 13)).map((row) => {
    const verdict = verdictOrNull(row[6] ?? "");
    const claim = row[5];
    if (verdict === null || !["true", "false"].includes(row[11] ?? "") ||
        (claim !== "compiler" && claim !== "behavior" && claim !== "artifact")) {
      throw new UserError("ledger.tsv has an invalid verdict, claim or independence value");
    }
    return { repo: row[0]!, pr: row[1]!, sha: row[2]!, base: row[3]!, inputs: row[4]!, claim,
      verdict, evidence: row[7]!, verifier: row[8]!, author: row[9]!, method: row[10]!,
      independent: row[11] === "true", ts: row[12]! };
  });
}

function ledgerCells(row: LedgerEntry): readonly string[] {
  return [row.repo, row.pr, row.sha, row.base, row.inputs, row.claim, row.verdict,
    row.evidence, row.verifier, row.author, row.method, String(row.independent), row.ts];
}

async function saveLedger(
  store: string,
  rows: readonly LedgerEntry[]
): Promise<void> {
  await writeTsv(
    join(store, "ledger.tsv"),
    LEDGER_HEADER,
    rows.map(ledgerCells)
  );
}

function pointerCells(pointer: InboxPointer): readonly string[] {
  return [
    pointer.ts,
    pointer.agent,
    pointer.unit,
    pointer.status,
    pointer.report,
  ];
}

async function readPointers(
  directory: string
): Promise<readonly InboxPointer[]> {
  let entries: Dirent[];
  try {
    entries = await readdir(directory, { withFileTypes: true });
  } catch (error) {
    if (errorCode(error) === "ENOENT") {
      throw new UserError(
        `store is not initialized at ${dirname(directory)}; run orch init`
      );
    }
    throw error;
  }
  const result: InboxPointer[] = [];
  const files = entries
    .filter((entry) => entry.isFile() && entry.name.endsWith(".tsv"))
    .sort((left, right) => left.name.localeCompare(right.name));
  for (const entry of files) {
    const raw = (await readFile(join(directory, entry.name), "utf8")).replace(
      /\r?\n$/,
      ""
    );
    const row = raw.split("\t");
    if (/[\r\n]/.test(raw) || row.length !== 5) {
      throw new UserError(`inbox pointer ${entry.name} is malformed`);
    }
    result.push({
      id: entry.name,
      ts: row[0] ?? "",
      agent: row[1] ?? "",
      unit: row[2] ?? "",
      status: row[3] ?? "",
      report: row[4] ?? "",
    });
  }
  return result;
}

function renderGates(rows: readonly Gate[]): string {
  if (rows.length === 0) {
    return "";
  }
  const blocks = rows.map((gate) => {
    const answer =
      gate.kind === "resolved" ? `\n- Answer: ${gate.answer}` : "";
    return `## ${gate.id}

- Status: ${gate.kind}
- Question: ${gate.question}
- Options: ${gate.options}
- Default: ${gate.defaultAnswer}${answer}`;
  });
  return `# Gates\n\n${blocks.join("\n\n")}\n`;
}

async function readGates(store: string): Promise<readonly Gate[]> {
  const raw = (await requiredFile(join(store, "gates.md")))
    .replace(/\r/g, "")
    .trim();
  if (raw.length === 0) {
    return [];
  }
  const prefix = "# Gates\n\n## ";
  if (!raw.startsWith(prefix)) {
    throw new UserError("gates.md has an invalid heading");
  }
  const result: Gate[] = [];
  for (const block of raw.slice(prefix.length).split("\n\n## ")) {
    const lines = block.split("\n").filter((value) => value.length > 0);
    const id = lines.shift() ?? "";
    const fields = new Map<string, string>();
    for (const value of lines) {
      const match = /^- ([^:]+): (.*)$/.exec(value);
      if (match === null) {
        throw new UserError(`gates.md has a malformed gate ${id}`);
      }
      fields.set(match[1] ?? "", match[2] ?? "");
    }
    const status = fields.get("Status");
    const question = fields.get("Question");
    const options = fields.get("Options");
    const defaultAnswer = fields.get("Default");
    if (
      id.length === 0 ||
      question === undefined ||
      options === undefined ||
      defaultAnswer === undefined
    ) {
      throw new UserError(`gates.md has a malformed gate ${id}`);
    }
    if (status === "open") {
      result.push({ kind: "open", id, question, options, defaultAnswer });
    } else if (status === "resolved" && fields.has("Answer")) {
      result.push({
        kind: "resolved",
        id,
        question,
        options,
        defaultAnswer,
        answer: fields.get("Answer") ?? "",
      });
    } else {
      throw new UserError(`gates.md has invalid status ${status ?? ""}`);
    }
  }
  if (new Set(result.map((gate) => gate.id)).size !== result.length) {
    throw new UserError("gates.md has duplicate gate ids");
  }
  return result;
}

function parseFrontier(raw: string): Frontier {
  let value: unknown;
  try {
    value = JSON.parse(raw);
  } catch {
    throw new UserError("frontier.json is not valid JSON");
  }
  if (!isRecord(value)) {
    throw new UserError("frontier.json must contain an object");
  }
  if (Object.keys(value).length === 0) {
    return { repo: "", trunk: "", observedAt: "", generation: 0, prs: [], lowestUnmerged: null };
  }
  if (
    typeof value.repo !== "string" || typeof value.trunk !== "string" ||
    typeof value.observedAt !== "string" ||
    typeof value.generation !== "number" ||
    !Number.isSafeInteger(value.generation) ||
    value.generation < 0 ||
    !isUnknownArray(value.prs) ||
    !(
      value.lowestUnmerged === null ||
      (typeof value.lowestUnmerged === "number" &&
        Number.isSafeInteger(value.lowestUnmerged))
    )
  ) {
    throw new UserError("frontier.json has an invalid shape");
  }
  const prs: FrontierPr[] = [];
  for (const row of value.prs) {
    const state = isRecord(row)
      ? frontierPrStateOrNull(row.state)
      : null;
    if (
      !isRecord(row) ||
      typeof row.pr !== "number" ||
      !Number.isSafeInteger(row.pr) ||
      row.pr < 1 ||
      typeof row.branches !== "string" ||
      row.branches.length === 0 ||
      typeof row.sha !== "string" || !/^[a-f0-9]{40,64}$/i.test(row.sha) ||
      typeof row.base !== "string" || row.base.length === 0 ||
      typeof row.baseSha !== "string" || !/^[a-f0-9]{40,64}$/i.test(row.baseSha) ||
      state === null
    ) {
      throw new UserError("frontier.json has an invalid PR row");
    }
    prs.push({
      pr: row.pr,
      branches: row.branches,
      sha: row.sha,
      base: row.base,
      baseSha: row.baseSha,
      state,
    });
  }
  if (value.lowestUnmerged !== (prs.find((row) => row.state !== "MERGED")?.pr ?? null)) {
    throw new UserError("frontier.json skips an unmerged prerequisite");
  }
  return {
    repo: value.repo, trunk: value.trunk, observedAt: value.observedAt,
    generation: value.generation,
    prs,
    lowestUnmerged: value.lowestUnmerged,
  };
}

async function readFrontier(store: string): Promise<Frontier> {
  return parseFrontier(await requiredFile(join(store, "frontier.json")));
}

async function readStanding(
  store: string
): Promise<readonly StandingLine[]> {
  const raw = (await requiredFile(join(store, "preferences.md"))).replace(
    /\r/g,
    ""
  );
  if (raw.trim().length === 0) {
    return [];
  }
  const result: StandingLine[] = [];
  for (const value of raw.split("\n").filter((item) => item.length > 0)) {
    const match = /^([1-9]\d*)\. (.+)$/.exec(value);
    const number = Number(match?.[1] ?? 0);
    if (match === null || number !== result.length + 1) {
      throw new UserError("preferences.md has malformed numbering");
    }
    result.push({ number, line: match[2] ?? "" });
  }
  return result;
}

function countValues(values: readonly string[]): Counts {
  const result: Record<string, number> = {};
  for (const value of values) {
    result[value] = (result[value] ?? 0) + 1;
  }
  return Object.fromEntries(
    Object.entries(result).sort(([left], [right]) =>
      left.localeCompare(right)
    )
  );
}

function summarize(
  unitRows: readonly Unit[],
  ledgerRows: readonly LedgerEntry[],
  currentFrontier: Frontier,
  gateRows: readonly Gate[]
): StatusSummary {
  return {
    unitStates: countValues(unitRows.map((unit) => unit.state)),
    ledgerVerdicts: countValues(ledgerRows.map((row) => row.verdict)),
    frontierGeneration: currentFrontier.generation,
    openGateIds: gateRows
      .filter((gate): gate is OpenGate => gate.kind === "open")
      .map((gate) => gate.id)
      .sort(),
  };
}

function countRecord(value: unknown): Record<string, number> | null {
  if (!isRecord(value)) {
    return null;
  }
  const result: Record<string, number> = {};
  for (const [name, count] of Object.entries(value)) {
    if (
      typeof count !== "number" ||
      !Number.isSafeInteger(count) ||
      count < 0
    ) {
      return null;
    }
    result[name] = count;
  }
  return result;
}

function previousSummary(raw: string): StatusSummary | null {
  const match = /<!-- orch-summary (.+) -->/.exec(raw);
  if (match === null) {
    return null;
  }
  let value: unknown;
  try {
    value = JSON.parse(match[1] ?? "");
  } catch {
    return null;
  }
  if (
    !isRecord(value) ||
    typeof value.frontierGeneration !== "number" ||
    !isUnknownArray(value.openGateIds)
  ) {
    return null;
  }
  const unitStates = countRecord(value.unitStates);
  const ledgerVerdicts = countRecord(value.ledgerVerdicts);
  const openGateIds = value.openGateIds.filter(
    (item): item is string => typeof item === "string"
  );
  if (
    unitStates === null ||
    ledgerVerdicts === null ||
    openGateIds.length !== value.openGateIds.length
  ) {
    return null;
  }
  return {
    unitStates,
    ledgerVerdicts,
    frontierGeneration: value.frontierGeneration,
    openGateIds,
  };
}

function changed(before: StatusSummary | null, after: StatusSummary): string {
  if (before === null) {
    return "first render";
  }
  const result: string[] = [];
  const groups: readonly {
    readonly label: string;
    readonly oldCounts: Counts;
    readonly newCounts: Counts;
  }[] = [
    {
      label: "units",
      oldCounts: before.unitStates,
      newCounts: after.unitStates,
    },
    {
      label: "ledger",
      oldCounts: before.ledgerVerdicts,
      newCounts: after.ledgerVerdicts,
    },
  ];
  for (const { label, oldCounts, newCounts } of groups) {
    const names = [
      ...new Set([...Object.keys(oldCounts), ...Object.keys(newCounts)]),
    ].sort();
    for (const name of names) {
      const oldCount = oldCounts[name] ?? 0;
      const newCount = newCounts[name] ?? 0;
      if (oldCount !== newCount) {
        result.push(`${label} ${name} ${oldCount}->${newCount}`);
      }
    }
  }
  if (before.frontierGeneration !== after.frontierGeneration) {
    result.push(
      `frontier generation ${before.frontierGeneration}->${after.frontierGeneration}`
    );
  }
  if (before.openGateIds.join("\0") !== after.openGateIds.join("\0")) {
    result.push(
      `open gates ${before.openGateIds.length}->${after.openGateIds.length}`
    );
  }
  return result.length === 0 ? "no derived changes" : result.join("; ");
}

function markdown(value: string): string {
  return value.replace(/\\/g, "\\\\").replace(/\|/g, "\\|");
}

function table(
  headers: readonly string[],
  rows: readonly (readonly string[])[]
): string {
  if (rows.length === 0) {
    return "(none)";
  }
  return [
    `| ${headers.join(" | ")} |`,
    `| ${headers.map(() => "---").join(" | ")} |`,
    ...rows.map((row) => `| ${row.map(markdown).join(" | ")} |`),
  ].join("\n");
}

function statusMarkdown(
  unitRows: readonly Unit[],
  ledgerRows: readonly LedgerEntry[],
  currentFrontier: Frontier,
  gateRows: readonly Gate[],
  currentSummary: StatusSummary
): string {
  return `# Orchestrate status

Generated: ${new Date().toISOString()}

## Units

States: ${countLine(currentSummary.unitStates)}

${table(
  ["ID", "Track", "State", "Branch", "PR", "SHA", "Brief"],
  unitRows.map(unitCells)
)}

## Verification ledger

Verdicts: ${countLine(currentSummary.ledgerVerdicts)}

${table(
  ["PR", "SHA", "Verdict", "Evidence", "Verifier", "Timestamp"],
  ledgerRows.map(ledgerCells)
)}

## Frontier

Generation: ${currentFrontier.generation}
Lowest unmerged: ${currentFrontier.lowestUnmerged ?? "none"}

${table(
  ["Branch", "PR", "SHA", "State"],
  currentFrontier.prs.map((row) => [
    row.branches,
    String(row.pr),
    row.sha,
    row.state,
  ])
)}

## Gates

${table(
  ["ID", "Status", "Question", "Options", "Default", "Answer"],
  gateRows.map((gate) => [
    gate.id,
    gate.kind,
    gate.question,
    gate.options,
    gate.defaultAnswer,
    gate.kind === "resolved" ? gate.answer : "",
  ])
)}

<!-- orch-summary ${JSON.stringify(currentSummary)} -->
`;
}

function countLine(value: Counts): string {
  const entries = Object.entries(value);
  return entries.length === 0
    ? "none"
    : entries.map(([name, count]) => `${name}=${count}`).join(", ");
}

export type Forge = "gh";

export function parseForge(value: string): Forge {
  if (value !== "gh") throw new UserError("forge must be gh");
  return value;
}

interface ForgePullRequest {
  readonly pr: number;
  readonly state: FrontierPrState;
  readonly branch: string;
  readonly base: string;
  readonly sha: string;
  readonly baseSha: string;
}

function hash(value: string, label: string): string {
  if (!/^(?:[a-f0-9]{40}|[a-f0-9]{64})$/i.test(value)) {
    throw new UserError(`${label} must be a full commit SHA or input digest`);
  }
  return value.toLowerCase();
}

function jsonTool(command: string, args: readonly string[], repo: string): unknown {
  try { return JSON.parse(runTool({ command, args, repo })); }
  catch (error) { throw new UserError(`${command} observation failed: ${errorMessage(error)}`); }
}

function repository(repo: string): { name: string; trunk: string } {
  const value = jsonTool("gh", ["repo", "view", "--json", "nameWithOwner,defaultBranchRef"], repo);
  if (!isRecord(value) || typeof value.nameWithOwner !== "string" ||
      !isRecord(value.defaultBranchRef) || typeof value.defaultBranchRef.name !== "string" ||
      !value.nameWithOwner.includes("/") || !value.defaultBranchRef.name) {
    throw new UserError("GitHub repository/default branch is unresolved");
  }
  return { name: value.nameWithOwner, trunk: value.defaultBranchRef.name };
}

const PR_FIELDS = "number,state,headRefName,baseRefName,headRefOid,baseRefOid";
function parsePullRequest(row: unknown): ForgePullRequest {
  if (!isRecord(row) || typeof row.number !== "number" || !Number.isSafeInteger(row.number) || row.number < 1 ||
      frontierPrStateOrNull(row.state) === null || typeof row.headRefName !== "string" || !row.headRefName ||
      typeof row.baseRefName !== "string" || !row.baseRefName ||
      typeof row.headRefOid !== "string" || typeof row.baseRefOid !== "string") {
    throw new UserError("GitHub returned an invalid or unknown PR observation");
  }
  return { pr: row.number, state: row.state as FrontierPrState,
    branch: row.headRefName, base: row.baseRefName,
    sha: hash(row.headRefOid, "observed head"), baseSha: hash(row.baseRefOid, "observed base") };
}

function runTool({
  command,
  args,
  repo,
}: {
  command: string;
  args: readonly string[];
  repo: string;
}): string {
  try {
    return execFileSync(command, args, {
      cwd: repo,
      encoding: "utf8",
      env: process.env,
      stdio: ["ignore", "pipe", "pipe"],
    });
  } catch (error) {
    throw new UserError(
      `${command} ${args.join(" ")} failed: ${errorMessage(error)}`
    );
  }
}

function listPullRequests({ filter, branch, repo }: {
  filter: "--head" | "--base"; branch: string; forge: Forge; repo: string;
}): readonly ForgePullRequest[] {
  const value = jsonTool("gh", ["pr", "list", filter, branch, "--state", "all", "--limit", "1000", "--json", PR_FIELDS], repo);
  if (!isUnknownArray(value) || value.length >= 1000) {
    throw new UserError("GitHub PR list is invalid or may be truncated");
  }
  return value.map(parsePullRequest);
}

function branchPullRequest({
  branch,
  forge,
  repo,
}: {
  branch: string;
  forge: Forge;
  repo: string;
}): ForgePullRequest {
  const rows = listPullRequests({ filter: "--head", branch, forge, repo });
  const row = rows[0];
  if (row === undefined) {
    throw new UserError(
      `branch ${branch} has no pull request; open one with ${forge} pr create, or resolve the frontier from a branch whose chain is fully submitted`
    );
  }
  if (rows.length > 1) {
    throw new UserError(
      `branch ${branch} has multiple pull requests (${rows.map((item) => `#${item.pr}`).join(", ")}); close the stale ones first`
    );
  }
  return row;
}

function openChildren({ branch, forge, repo }: {
  branch: string; forge: Forge; repo: string;
}): readonly ForgePullRequest[] {
  const cache = new Map<string, readonly ForgePullRequest[]>();
  function children(name: string): readonly ForgePullRequest[] {
    let rows = cache.get(name);
    if (rows === undefined) { rows = listPullRequests({ filter: "--base", branch: name, forge, repo }); cache.set(name, rows); }
    return rows;
  }
  function active(row: ForgePullRequest, trail: ReadonlySet<string>): boolean {
    if (trail.has(row.branch) || trail.size > 1000) throw new UserError("PR descendant chain loops or exceeds discovery limit");
    if (row.state === "OPEN") return true;
    const next = new Set([...trail, row.branch]);
    return children(row.branch).some((child) => active(child, next));
  }
  return children(branch).filter((row) => active(row, new Set([branch])));
}

function runGit({
  args,
  repo,
}: {
  args: readonly string[];
  repo: string;
}): string {
  return runTool({ command: "git", args, repo }).trim();
}

function currentBranch(repo: string): string {
  return runGit({ args: ["symbolic-ref", "--short", "HEAD"], repo });
}

function resolveFrontier(repo: string, forge: Forge, trunk: string): readonly FrontierPr[] {
  const current = currentBranch(repo);
  if (current === trunk) {
    return [];
  }
  const seen = new Set([current]);
  let bottom = branchPullRequest({ branch: current, forge, repo });
  const chain = [bottom];
  while (bottom.base !== trunk) {
    if (seen.has(bottom.base)) {
      throw new UserError(
        `pull request base chain loops at branch ${bottom.base}`
      );
    }
    seen.add(bottom.base);
    bottom = branchPullRequest({ branch: bottom.base, forge, repo });
    chain.unshift(bottom);
  }
  let tip = current;
  for (;;) {
    const children = openChildren({ branch: tip, forge, repo });
    const child = children[0];
    if (child === undefined) {
      break;
    }
    if (children.length > 1) {
      throw new UserError(
        `stack is not linear: branches ${children.map((row) => row.branch).join(", ")} all target ${tip}`
      );
    }
    if (seen.has(child.branch)) {
      throw new UserError(
        `pull request base chain loops at branch ${child.branch}`
      );
    }
    seen.add(child.branch);
    chain.push(child);
    tip = child.branch;
  }
  if (new Set(chain.map((row) => row.pr)).size !== chain.length) {
    throw new UserError(
      "pull request base chain contains duplicate pull requests"
    );
  }
  for (let i = 1; i < chain.length; i++) {
    if (chain[i]!.baseSha !== chain[i - 1]!.sha) throw new UserError("PR head/base observations drifted; refresh the frontier");
  }
  return chain.map((row) => ({
    pr: row.pr,
    branches: row.branch,
    sha: row.sha,
    base: row.base,
    baseSha: row.baseSha,
    state: row.state,
  }));
}

function validateFrontierPin({
  actual,
  expected,
}: {
  actual: readonly number[];
  expected: readonly number[];
}): void {
  if (
    actual.length === expected.length &&
    actual.every((pr, index) => pr === expected[index])
  ) {
    return;
  }
  const actualSet = new Set(actual);
  const expectedSet = new Set(expected);
  const missing = expected.filter((pr) => !actualSet.has(pr));
  const extra = actual.filter((pr) => !expectedSet.has(pr));
  const drift: string[] = [];
  if (missing.length > 0) {
    drift.push(`missing from resolved chain: ${missing.join(",")}`);
  }
  if (extra.length > 0) {
    drift.push(`extra in resolved chain: ${extra.join(",")}`);
  }
  if (missing.length === 0 && extra.length === 0) {
    drift.push(
      `order differs: expected ${expected.join(",")}; resolved ${actual.join(",")}`
    );
  }
  throw new UserError(`frontier pin mismatch: ${drift.join("; ")}`);
}

export function openStore(
  directory: string,
  options: OpenStoreOptions = {}
): Store {
  const store = resolve(directory);
  let closed = false;
  let releaseLock: (() => Promise<void>) | null = null;
  let lockRequest: Promise<void> | null = null;

  const ensureOpen = (): void => {
    if (closed) {
      throw new UserError("store is closed");
    }
  };

  const ensureLock = async (): Promise<void> => {
    ensureOpen();
    if (releaseLock !== null) {
      return;
    }
    if (lockRequest === null) {
      lockRequest = acquireLock(store, options).then((release) => {
        releaseLock = release;
      });
    }
    try {
      await lockRequest;
    } catch (error) {
      lockRequest = null;
      throw error;
    }
  };

  const beginWrite = async (): Promise<void> => {
    ensureOpen();
    if (!(await exists(store))) {
      throw new UserError(
        `store is not initialized at ${store}; run orch init`
      );
    }
    await ensureLock();
  };

  return {
    units: {
      add: async (params) => {
        await beginWrite();
        const row: Unit = {
          id: requiredCell(params.id, "unit id"),
          track: requiredCell(params.track, "track"),
          state: "pending",
          branch: "",
          pr: "",
          sha: "",
          brief:
            params.brief === undefined
              ? ""
              : requiredCell(params.brief, "brief"),
        };
        const rows = [...(await readUnits(store))];
        if (rows.some((unit) => unit.id === row.id)) {
          throw new UserError(`unit ${row.id} already exists`);
        }
        rows.push(row);
        await saveUnits(store, rows);
        return row;
      },
      set: async (params) => {
        await beginWrite();
        const id = requiredCell(params.id, "unit id");
        const state = requiredCell(params.state, "state");
        const rows = [...(await readUnits(store))];
        const index = rows.findIndex((unit) => unit.id === id);
        const old = rows[index];
        if (index < 0 || old === undefined) {
          throw new NotFoundError(`unit ${id} not found`);
        }
        const row: Unit = {
          ...old,
          state,
          branch:
            params.branch === undefined
              ? old.branch
              : requiredCell(params.branch, "branch"),
          pr:
            params.pr === undefined
              ? old.pr
              : String(positiveInteger(params.pr, "PR")),
          sha:
            params.sha === undefined
              ? old.sha
              : requiredCell(params.sha, "SHA"),
        };
        rows[index] = row;
        await saveUnits(store, rows);
        return row;
      },
      get: async (id) => {
        ensureOpen();
        const cleanId = requiredCell(id, "unit id");
        const row = (await readUnits(store)).find(
          (unit) => unit.id === cleanId
        );
        if (row === undefined) {
          throw new NotFoundError(`unit ${cleanId} not found`);
        }
        return row;
      },
      list: async (params = {}) => {
        ensureOpen();
        const state =
          params.state === undefined
            ? undefined
            : requiredCell(params.state, "state");
        const track =
          params.track === undefined
            ? undefined
            : requiredCell(params.track, "track");
        return (await readUnits(store)).filter(
          (unit) =>
            (state === undefined || unit.state === state) &&
            (track === undefined || unit.track === track)
        );
      },
      counts: async () => {
        ensureOpen();
        return countValues(
          (await readUnits(store)).map((unit) => unit.state)
        );
      },
    },
    ledger: {
      record: async (params) => {
        await beginWrite();
        const row: LedgerEntry = {
          repo: requiredLine(params.repo, "repository name"),
          pr: String(positiveInteger(params.pr, "PR")),
          sha: hash(params.sha, "head"), base: hash(params.base, "base"),
          inputs: hash(params.inputs, "input digest"), claim: params.claim, verdict: parseVerdict(params.verdict),
          evidence: requiredCell(params.evidence, "evidence"),
          verifier: requiredCell(params.verifier, "verifier"), author: requiredCell(params.author, "author"),
          method: requiredCell(params.method, "method"), independent: params.independent ?? false,
          ts: new Date().toISOString(),
        };
        if (!row.repo.includes("/") || (row.independent && row.verifier.toLowerCase() === row.author.toLowerCase())) {
          throw new UserError("independent proof needs a distinct verifier and a repository name");
        }
        if (!["compiler", "behavior", "artifact"].includes(row.claim) ||
            (row.verdict.endsWith("-verified") && row.verdict !== `${row.claim}-verified`)) {
          throw new UserError("verification verdict does not prove the declared claim");
        }
        if (!(await exists(resolve(store, row.evidence)))) throw new UserError("verification receipt is missing");
        const rows = [...await readLedger(store)];
        const prior = rows.filter((old) => old.repo === row.repo && old.pr === row.pr && old.sha === row.sha &&
          old.base === row.base && old.inputs === row.inputs && old.claim === row.claim &&
          old.independent === row.independent).at(-1);
        const same = prior !== undefined && ledgerCells(prior).slice(0, -1).join("\t") === ledgerCells(row).slice(0, -1).join("\t") ? prior : undefined;
        if (same === undefined) { rows.push(row); await saveLedger(store, rows); }
        return same ?? row;
      },
      check: async (params) => {
        ensureOpen();
        const repo = resolve(requiredLine(params.repo, "repo directory"));
        const identity = repository(repo);
        const pr = positiveInteger(params.pr, "PR");
        const observed = parsePullRequest(jsonTool("gh", ["pr", "view", String(pr), "--json", PR_FIELDS], repo));
        const sha = hash(params.sha, "head"), base = hash(params.base, "base"), inputs = hash(params.inputs, "input digest");
        if (observed.pr !== pr || observed.state !== "OPEN" || observed.sha !== sha || observed.baseSha !== base) {
          throw new UserError("PR state/head/base changed; obtain current evidence before landing");
        }
        const matching = (await readLedger(store)).filter((row) => row.repo === identity.name && row.pr === String(pr) &&
          row.sha === sha && row.base === base && row.inputs === inputs && row.claim === params.claim);
        const row = matching.filter((entry) => entry.independent).at(-1);
        if (row === undefined) throw new NotFoundError("NOT-VERIFIED", {
          compact: "NOT-VERIFIED", json: { repo: identity.name, pr, sha, base, verdict: "NOT-VERIFIED" },
        });
        if (row.verdict === "verifier-failed" || row.verdict === "verifier-blocked") {
          throw new UserError(`independent verdict is ${row.verdict}`);
        }
        if (!(await exists(resolve(store, row.evidence)))) throw new UserError("verification receipt is missing");
        return row;
      },
      summary: async () => { ensureOpen(); return countValues((await readLedger(store)).map((row) => row.verdict)); },
    },
    inbox: {
      push: async (params) => {
        await beginWrite();
        const ts = new Date().toISOString();
        const filename = `${ts.replace(/[:.]/g, "-")}-${process.pid}-${randomUUID()}.tsv`;
        const pointer: InboxPointer = {
          id: filename,
          ts,
          agent: requiredCell(params.agent, "agent"),
          unit: requiredCell(params.unit, "unit"),
          status: requiredCell(params.status, "status"),
          report:
            params.report === undefined
              ? ""
              : requiredCell(params.report, "report"),
        };
        const inbox = join(store, "inbox");
        if (!(await exists(inbox))) {
          throw new UserError(
            `store is not initialized at ${store}; run orch init`
          );
        }
        const contents = `${pointerCells(pointer).map(cleanCell).join("\t")}\n`;
        await atomicWrite(join(inbox, filename), contents);
        return { pointer, filename };
      },
      drain: async () => {
        await beginWrite();
        const batches = join(store, "batches");
        await mkdir(batches, { recursive: true });
        await mkdir(join(store, "inbox"), { recursive: true });
        const pending = (await readdir(batches)).sort()[0];
        if (pending !== undefined) return { id: pending, pointers: await readPointers(join(batches, pending)) };
        const inbox = join(store, "inbox");
        await mkdir(inbox, { recursive: true });
        if ((await readPointers(inbox)).length === 0) return { id: null, pointers: [] };
        const id = randomUUID();
        await rename(inbox, join(batches, id));
        await mkdir(inbox, { recursive: true });
        return { id, pointers: await readPointers(join(batches, id)) };
      },
      ack: async (id, receipt) => {
        await beginWrite();
        if (!/^[a-f0-9-]{36}$/.test(id)) throw new UserError("invalid batch id");
        const archived = join(store, "accounted", id);
        if (await exists(archived)) return { id, count: (await readPointers(archived)).length };
        const batch = join(store, "batches", id);
        const pointers = await readPointers(batch);
        let value: unknown;
        try { value = JSON.parse(await readFile(resolve(store, receipt), "utf8")); }
        catch { throw new UserError("accounting receipt must be readable JSON"); }
        if (!isRecord(value) || value.batch !== id || !isUnknownArray(value.entries) || value.entries.length !== pointers.length) {
          throw new UserError("accounting receipt must cover the complete batch");
        }
        const units = await readUnits(store), ledger = await readLedger(store);
        const covered = new Set<string>();
        for (const entry of value.entries) {
          if (!isRecord(entry) || typeof entry.pointer !== "string" || covered.has(entry.pointer)) throw new UserError("invalid/duplicate pointer receipt");
          const pointer = pointers.find((item) => item.id === entry.pointer);
          if (pointer === undefined) throw new UserError("unknown pointer receipt");
          covered.add(entry.pointer);
          if (entry.action === "unit") {
            if (!units.some((row) => row.id === pointer.unit && row.state === entry.state)) throw new UserError("unit result is not saved");
          } else if (entry.action === "ledger") {
            if (!units.some((unit) => unit.id === pointer.unit && unit.pr === String(entry.pr) && unit.sha === entry.sha) ||
                !ledger.some((row) => row.repo === entry.repo && row.pr === String(entry.pr) && row.sha === entry.sha &&
                row.base === entry.base && row.inputs === entry.inputs && row.claim === entry.claim && row.verifier === entry.verifier)) throw new UserError("ledger result is not saved");
          } else if (entry.action !== "noise" || typeof entry.reason !== "string" || !entry.reason.trim()) {
            throw new UserError("pointer needs a saved unit/ledger result or an explicit noise reason");
          }
        }
        await atomicWrite(join(batch, "accounting.json"), JSON.stringify({ receipt: value, units, ledger }, null, 2) + "\n");
        await mkdir(join(store, "accounted"), { recursive: true });
        await rename(batch, archived);
        return { id, count: pointers.length };
      },
      peek: async () => {
        ensureOpen();
        const rows: InboxPointer[] = [];
        const batches = join(store, "batches");
        if (await exists(batches)) for (const id of (await readdir(batches)).sort()) rows.push(...await readPointers(join(batches, id)));
        rows.push(...await readPointers(join(store, "inbox")));
        return rows;
      },
      count: async () => {
        ensureOpen();
        let count = (await readPointers(join(store, "inbox"))).length;
        const batches = join(store, "batches");
        if (await exists(batches)) for (const id of await readdir(batches)) count += (await readPointers(join(batches, id))).length;
        return count;
      },
    },
    gates: {
      park: async (params) => {
        await beginWrite();
        const gate: OpenGate = {
          kind: "open",
          id: requiredLine(params.id, "gate id"),
          question: requiredLine(params.question, "question"),
          options: requiredLine(params.options, "options"),
          defaultAnswer: requiredLine(
            params.defaultAnswer,
            "default"
          ),
        };
        const rows = [...(await readGates(store))];
        const index = rows.findIndex((old) => old.id === gate.id);
        if (index < 0) {
          rows.push(gate);
        } else {
          rows[index] = gate;
        }
        await atomicWrite(join(store, "gates.md"), renderGates(rows));
        return gate;
      },
      list: async () => {
        ensureOpen();
        return (await readGates(store)).filter(
          (gate): gate is OpenGate => gate.kind === "open"
        );
      },
      resolve: async (params) => {
        await beginWrite();
        const id = requiredLine(params.id, "gate id");
        const rows = [...(await readGates(store))];
        const index = rows.findIndex((gate) => gate.id === id);
        const old = rows[index];
        if (index < 0 || old === undefined) {
          throw new NotFoundError(`gate ${id} not found`);
        }
        const gate: ResolvedGate = {
          kind: "resolved",
          id: old.id,
          question: old.question,
          options: old.options,
          defaultAnswer: old.defaultAnswer,
          answer: requiredLine(params.answer, "answer"),
        };
        rows[index] = gate;
        await atomicWrite(join(store, "gates.md"), renderGates(rows));
        return gate;
      },
    },
    frontier: {
      set: async (params) => {
        await beginWrite();
        const repo = resolve(requiredLine(params.repo, "repo directory"));
        const forge = parseForge(params.forge ?? "gh");
        const pin =
          params.prs === undefined
            ? undefined
            : params.prs.map((pr) => positiveInteger(pr, "PR"));
        if (pin !== undefined && new Set(pin).size !== pin.length) {
          throw new UserError("--prs must not contain duplicates");
        }
        const old = await readFrontier(store);
        const identity = repository(repo);
        const prs = resolveFrontier(repo, forge, identity.trunk);
        if (pin !== undefined) {
          validateFrontierPin({
            actual: prs.map((row) => row.pr),
            expected: pin,
          });
        }
        const value: Frontier = {
          repo: identity.name, trunk: identity.trunk, observedAt: new Date().toISOString(),
          generation: old.generation + 1,
          prs,
          lowestUnmerged: prs.find((row) => row.state !== "MERGED")?.pr ?? null,
        };
        await atomicWrite(
          join(store, "frontier.json"),
          `${JSON.stringify(value, null, 2)}\n`
        );
        return value;
      },
      show: async () => {
        ensureOpen();
        return readFrontier(store);
      },
    },
    standing: {
      show: async () => {
        ensureOpen();
        return readStanding(store);
      },
      add: async (params) => {
        await beginWrite();
        const rows = [...(await readStanding(store))];
        const item: StandingLine = {
          number: rows.length + 1,
          line: requiredLine(params.line, "standing order"),
        };
        rows.push(item);
        await atomicWrite(
          join(store, "preferences.md"),
          `${rows.map((row) => `${row.number}. ${row.line}`).join("\n")}\n`
        );
        return item;
      },
    },
    status: {
      render: async () => {
        await beginWrite();
        const unitRows = await readUnits(store);
        const ledgerRows = await readLedger(store);
        const currentFrontier = await readFrontier(store);
        const gateRows = await readGates(store);
        const currentSummary = summarize(
          unitRows,
          ledgerRows,
          currentFrontier,
          gateRows
        );
        const path = join(store, "status.md");
        const before = (await exists(path))
          ? previousSummary(await readFile(path, "utf8"))
          : null;
        const change = changed(before, currentSummary);
        await atomicWrite(
          path,
          statusMarkdown(
            unitRows,
            ledgerRows,
            currentFrontier,
            gateRows,
            currentSummary
          )
        );
        return {
          units: unitRows,
          ledger: ledgerRows,
          frontier: currentFrontier,
          gates: gateRows,
          summary: currentSummary,
          changed: change,
        };
      },
    },
    init: async () => {
      ensureOpen();
      await mkdir(store, { recursive: true });
      await ensureLock();
      await writeIfMissing(join(store, "units.tsv"), `${UNIT_HEADER}\n`);
      await writeIfMissing(join(store, "ledger.tsv"), `${LEDGER_HEADER}\n`);
      await mkdir(join(store, "inbox"), { recursive: true });
      await writeIfMissing(join(store, "gates.md"), "");
      await writeIfMissing(join(store, "preferences.md"), "");
      await writeIfMissing(join(store, "frontier.json"), "{}\n");
      return { store };
    },
    close: async () => {
      if (closed) {
        return;
      }
      if (lockRequest !== null) {
        try {
          await lockRequest;
        } catch {
        }
      }
      const release = releaseLock;
      releaseLock = null;
      closed = true;
      if (release !== null) {
        await release();
      }
    },
  };
}
