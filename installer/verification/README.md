# Verify installer changes locally

Run the maintained entrypoint from the repository root. This initial verification
foundation does not install NeoHaskell or claim platform compatibility.

```sh
python3 installer/verification/verify.py self-test
python3 installer/verification/verify.py plan
```

The self-test uses disposable Git repositories to check the verifier and local
push gate. The plan lists required platform cases. It exits with code 2 while
real adapters, support floors, or conditional cases remain unresolved.

After committing the intended verifier source, install the repository hook with
`python3 installer/verification/verify.py setup-hooks`. Re-run setup after changing
the verifier policy. Hook setup must preserve existing hooks and refuses an
unsupported configuration. Git permits deliberate bypass with `--no-verify`.
This local gate provides no server enforcement.

For a staged candidate, use the committed source and exact manifest with the
required real adapters.

```sh
python3 installer/verification/verify.py run \
  --source <full-commit-id> \
  --candidate <manifest.json>
```
The current candidate format is provisional. A successful fixture result cannot
produce complete production acceptance while this registry remains a scaffold.

## Required lanes

Run the normal install, product, and lifecycle behavior on every lane in `coverage.json`.

| Lane | Required environment |
| --- | --- |
| `linux-x86_64` | Linux x86-64 with active systemd and the standard multiuser daemon. |
| `linux-arm64` | Linux ARM64 with active systemd and the standard multiuser daemon. |
| `macos-arm64` | Apple silicon macOS using launchd. |
| `wsl2-x86_64` | Native Windows PowerShell and actual `wsl.exe` dispatch to x86-64 WSL2. |
| `wsl2-arm64` | Native Windows PowerShell and actual `wsl.exe` dispatch to ARM64 WSL2. |

Start with a compatible multi-user daemon, an unrelated setting, and neither
project cache configured. Compare the installer's review with the changes it
makes after approval. Rerun as an ordinary user and verify that no cache setup,
administrator prompt, or daemon restart occurs. For administrator-managed
systems, use [Configure Nix for NeoHaskell](../../neo/nix-setup.md).
The declarative configuration preservation cases cover
NixOS on both Linux lanes and nix-darwin on macOS. The WSL lanes also require
native Windows selection, dispatch, and boundary messaging checks.

The shared case list covers clean-client cache substitution, separate lockstep commands, fresh-shell PATH resolution, the installed `neo` project flow, paired updates and rollback, fallback lifecycle operations, uninstall preservation, failure recovery, terminal behavior, timing, and private diagnostics.

## Conditional Nix variants

`coverage.json` lists official Nix, Determinate, and Lix as conditional cases on each lane. Run a variant case wherever the published support matrix promises that variant. Record the support decision and tested version in the evidence. Unknown applicability keeps acceptance incomplete.

The registry defines no operating-system or Nix version floor. Derive each floor from pinned artifacts and real platform evidence before publishing support.

## Evidence limits

Fixtures test the fixture and its adapter. A fixture result never proves platform acceptance, clean-client compatibility, actual cache substitution, native Windows dispatch, or production behavior. Cross-compilation also does not prove installation or lifecycle behavior.

Record skipped and unavailable cases as incomplete. Keep real clean-client runs separate from controlled fault injection. Run an OS-specific failure check on the OS that owns the boundary. Do not count a mocked `wsl.exe` as Windows routing evidence.

## Adapter and runner status

The entrypoint exists. Real installation adapters and candidate publication remain
unimplemented in this foundation. Platform acceptance requires installation,
lifecycle, cache, and generated-project evidence from real adapters.

Inventory configured runners and adapters during implementation. Record unavailable environments and retain their required cases as incomplete. A container or guest that cannot reproduce the required service owner, native Windows dispatch, or clean-client state does not satisfy the corresponding lane.

## Maintain shared-file classification

The development-shell exemption recognizes reviewed root-flake projections and
exact package recipes in `inputs.json`. A changed structure or recipe retains all
root-flake tokens. Recheck package independence before approving another shape.
The Hix configuration similarly exempts known shell-only settings only for its
reviewed shape and exact dependency lock. Compiler and module settings remain
installation inputs. The root Cabal project is an input because it selects the
package graph used to build neoctl. The development-only Cabal project is not an
installation input. Both exemptions check the other file's reviewed package
structure. A structural change disables the opposite exemption until reviewed.
The nhcore Cabal manifest is also a direct package-build input. Ordinary framework
source edits remain outside this classifier. New candidate artifacts still need
their own matching acceptance record.

Changing the classifier, registry, runners, or fixtures requires fresh evidence.
