# ADR-0081: Scope temporary video files to a private workspace

## Status

Proposed

## Context

Video extraction currently places input and output files in the shared temporary directory. A media manifest can therefore resolve an unrelated sibling there. UUID filenames do not isolate the directory. Core `Directory.withTemporaryFolder` creates an owner-only folder and removes it after every exit.

## Decision

Run the whole extraction inside Core `Directory.withTemporaryFolder`. It creates an owner-only directory atomically on POSIX and removes it after success, typed errors, runtime exceptions and cancellation. Keep fixed input/output filenames inside that owned directory. Add no library dependency and no dialect exemption.

Preserve all public types, defaults and accepted formats. Do not introduce an input-format or protocol allowlist. Do not change Core subprocess ownership here; Change023 supplies that separate prerequisite.

## Consequences

The demonstrated shared-directory sibling exposure is removed and temporary files are cleaned up after the extraction scope exits. POSIX protection assumes a trusted or sticky temporary parent and does not isolate processes running as the same user. OS cleanup failures can leave a private directory. This is not a sandbox: absolute and parent-relative references, network protocols and ffmpeg vulnerabilities remain outside this directory repair. No new package dependency, public configuration knob or media compatibility restriction is introduced.
