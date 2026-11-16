# Run the installer experience prototype

This is disposable decision evidence. Never merge this branch or use this code as the production installer.

Run from the repository root with Python 3.9 or later and uv.

```sh
uv run --with rich==15.0.0 python .agents-state/prototypes/installer-experience/prototype.py --variant 3
```

Press Enter to review the plan. Press Enter again to start the simulation. Press M to compare designs or S in the selector to change the scenario. D shows details. Q cancels or exits.

No Nix commands, network operations or system changes occur in the prototype. The uv command may download the Python rendering dependency. Transfer sizes, durations and successful installation checks are fictional.

## Repeat the terminal checks

```sh
uv run --with rich==15.0.0 python .agents-state/prototypes/installer-experience/probe.py
```

The PTY checks cover all three designs, details, failed downloads and retry, Ctrl+C, and a 42-column terminal. Redirected-output checks cover five scenarios and explicit noninteractive consent. The saved result is in `captures/probe-result.txt`. These checks passed on macOS. They do not prove production platform support, real Nix integration, actual cache use, screen-reader compatibility or live resize behavior.

Use `--plain` for plain output, `--still` to disable motion, and `--yes` to run the simulation without questions. Available scenarios are `new`, `existing`, `network`, `systemd` and `managed`.

## Accepted decision

Nick chose Dashboard after trying the alternatives, and confirmed that no further visual changes were needed. The first release ships in English. All installer-owned strings must be centralized in one file per language during implementation. This throwaway rendering code still has inline strings.

The installer shows one short review before system changes. Scripts require explicit noninteractive consent. Normal output stays brief with technical logs behind Details. Errors show their cause, partial state and recovery action. Motion stays within the active task. A still mode and equivalent plain output remain available. Byte progress requires measurable data. An overall percentage must not be invented.

Charm.land is the visual quality reference. Python and Rich are prototype tools, not selected production dependencies. Ratatui and Charm libraries remain implementation options. Distribution, update and uninstall mechanisms belong to the next decision. The retry fixture does not settle resumability or rollback semantics.

The prototype applies the accepted official-Nix and existing-Nix coexistence requirements. It does not test them. The independent comment review found no Python comments and requested no changes.

## Decision records

- [Installer experience decision](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/91fa5030-d1a9-43bb-aef6-c0d0009cf9d8/).
- [Platform and Nix coexistence decision](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/252f9169-74ef-4ef9-bea6-848aed15da4b/).
- [Charm visual reference](https://charm.land/).
