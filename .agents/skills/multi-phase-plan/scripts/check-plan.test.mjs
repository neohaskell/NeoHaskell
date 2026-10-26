import { mkdtempSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { join, resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import assert from "node:assert/strict";
import { test } from "node:test";

const checker = join(dirname(fileURLToPath(import.meta.url)), "check-plan.mjs");
const rootResult = spawnSync("git", ["rev-parse", "--show-toplevel"], { encoding: "utf8" });
assert.equal(rootResult.status, 0, rootResult.stderr);
const repositoryRoot = rootResult.stdout.trim();
const rule = "Tests alone are not sufficient verification. A PR is verified only when its unit, live, and perf boxes are all checked.";
const validPlan = `# Framework contract plan

## How to read this
One box is one unit of work and names the evidence. Check a box only when its evidence exists. See playbooks/shipping.md.
${rule}

## Program checklist
### Arm the program
- [ ] Trunk. Read \`git show origin/trunk:AGENTS.md\`.
- [ ] Audit cadence. Use available supervision and record each status message.
### Spawn owners
### PR mechanics
### Verdict and merge
### Boot recipe

## Preserve generated command behavior (PR 1)
**Depends on.** None.
**Files.**
- [ ] Edit the generator and its consumer fixture.
**Build.**
- [ ] Preserve the checked public contract.
**You see.**
- [ ] The consumer compiles and its expected result is observed.
**Verify, unit.** ${rule}
- [ ] Run the focused consumer and behavior tests.
**Verify, live.** ${rule} Ten lanes at the PR head, batched within capacity.
${Array.from({ length: 10 }, (_, i) => `- [ ] Lane ${i + 1}. Run consumer scenario ${i + 1}. Save \`.agents-state/lane-${i + 1}.txt\`. Pass when the expected compiler or runtime result is observed.`).join("\n")}
**Verify, perf.** ${rule}
- [ ] Metric. Consumer compilation seconds.
- [ ] Probe. Interleave matched clean builds on trunk and head.
- [ ] Baseline. Record the trunk median.
- [ ] Rule. Fail above the specified budget.
**Review gate.** Nonvisual. The operator reviews before merge.
- [ ] Present compiler and execution evidence to the operator.
**Merge.**
- [ ] Merge the verified head after approval under Shipping.

## Close the program
- [ ] Every box has its evidence.

## Appendix A. Prototype evidence
Record formal checks and their results.
`;

const runPlan = (source, expectedExit, diagnostic) => {
  const base = resolve(process.env.PLAN_TEST_DIR ?? join(repositoryRoot, ".agents-state/check-plan-tests"));
  mkdirSync(base, { recursive: true });
  const directory = mkdtempSync(join(base, "case-"));
  const file = join(directory, "plan.md");
  writeFileSync(file, source);
  try {
    const result = spawnSync(process.execPath, [checker, file], { encoding: "utf8" });
    assert.equal(result.status, expectedExit, result.stdout + result.stderr);
    if (diagnostic) assert.match(result.stderr, diagnostic);
    return result;
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
};

test("accepts a nonvisual framework plan without execution-setting or image requirements", () => {
  const result = runPlan(validPlan, 0);
  assert.match(result.stdout, /1 PR sections, 0 problems/);
});
test("accepts a visual gate with screenshots and video", () => {
  runPlan(validPlan.replace("Nonvisual. The operator", "Visual. The operator")
    .replace("Present compiler and execution evidence", "Present screenshot and video evidence"), 0);
});
test("accepts an explicitly ungated PR without review boxes", () => {
  runPlan(validPlan.replace("Nonvisual. The operator reviews before merge.\n- [ ] Present compiler and execution evidence to the operator.", "None. This PR changes no interaction."), 0);
});
for (const lane of [1, 5, 10]) {
  test(`rejects missing required lane ${lane}`, () => {
    runPlan(validPlan.replace(new RegExp(`^- \\[ \\] Lane ${lane}\\. .*\\n`, "m"), ""), 1, /expected 1 to 10/);
  });
}
test("rejects duplicate lane coverage", () => {
  runPlan(validPlan.replace("Lane 10.", "Lane 9."), 1, /expected 1 to 10/);
});
test("rejects a lane without a saved evidence artifact", () => {
  runPlan(validPlan.replace("Save `.agents-state/lane-3.txt`.", ""), 1, /lane 3 names no evidence artifact/);
});
test("rejects an unresolved lane evidence path", () => {
  runPlan(validPlan.replace(".agents-state/lane-3.txt", "<lane-evidence-path>"), 1, /lane 3 names no evidence artifact/);
});
test("rejects a lane without a pass predicate", () => {
  runPlan(validPlan.replace("Pass when the expected compiler or runtime result is observed.", ""), 1, /lane 1 has no pass predicate/);
});
test("rejects a missing performance baseline", () => {
  runPlan(validPlan.replace("- [ ] Baseline. Record the trunk median.\n", ""), 1, /expected \[Metric\., Probe\., Baseline\., Rule\.\]/);
});
test("rejects a missing verification gate rule", () => {
  runPlan(validPlan.replace(`**Verify, unit.** ${rule}`, "**Verify, unit.**"), 1, /does not open with the rule/);
});
test("rejects missing audit cadence", () => {
  runPlan(validPlan.replace("- [ ] Audit cadence. Use available supervision and record each status message.\n", ""), 1, /lacks "Audit cadence\."/);
});
test("rejects a visual gate without video evidence", () => {
  runPlan(validPlan.replace("Nonvisual.", "Visual.").replace("Present compiler and execution evidence", "Present screenshot evidence"), 1, /lacks "video"/);
});
test("rejects an undeclared review gate kind", () => {
  runPlan(validPlan.replace("Nonvisual.", "Later."), 1, /must declare Visual, Nonvisual or None/);
});
test("rejects a nonvisual gate without an operator review action", () => {
  runPlan(validPlan.replace("evidence to the operator", "evidence to the archive"), 1, /lacks "operator"/);
});
test("rejects review actions underneath a None gate", () => {
  runPlan(validPlan.replace("Nonvisual.", "None."), 1, /says None but has boxes/);
});
test("rejects reordered contract blocks", () => {
  runPlan(validPlan.replace("**Build.**", "**Temporary.**").replace("**You see.**", "**Build.**").replace("**Temporary.**", "**You see.**"), 1, /sub-blocks are/);
});
test("accepts ordinary, indented, and longer closing fences", () => {
  for (const indent of ["", " ", "  ", "   "])
    runPlan(validPlan + `${indent}\`\`\`markdown\n## Example only\n${indent}\`\`\`\n`, 0);
  runPlan(validPlan + "```markdown\n## Example only\n````\n", 0);
});
test("accepts nested backticks and tilde info strings", () => {
  runPlan(validPlan + "````markdown\n```markdown\n## Example only\n```\n````\n", 0);
  runPlan(validPlan + "~~~markdown `info`\n## Example only\n ~~~\n", 0);
});
test("keeps mismatched, shorter, and suffixed closers inside the fence", () => {
  for (const middle of ["~~~", "```", "``` info"])
    runPlan(validPlan + `\`\`\`\`markdown\n${middle}\n## Example only\n\`\`\`\`\n`, 0);
});
test("rejects headings and prose outside a proper close", () => {
  for (const suffix of ["  ", "\t"])
    runPlan(validPlan + `\`\`\`markdown\n## Example only\n\`\`\`${suffix}\n## Example only\n`, 1, /after Close the program is not an appendix/);
  runPlan(validPlan + "```markdown\nThis has a colon: inside code\n```\nThis has a colon: outside code\n", 1, /mid-sentence colon/);
});
test("rejects backtick info strings containing a backtick", () => {
  runPlan(validPlan + "```markdown `invalid`\n## Example only\n```\n", 1, /after Close the program is not an appendix/);
});
