//! T9 — the `uninterpreted-subprocess-error` GH issue template must exist and
//! contain the sections the `neo::subprocess_raw` help text directs users to fill.

use std::path::PathBuf;

fn template_path() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .join(".github")
        .join("ISSUE_TEMPLATE")
        .join("uninterpreted-subprocess-error.md")
}

#[test]
fn issue_template_file_exists_and_parses() {
    let path = template_path();
    let contents = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "failed to read GH issue template at {}: {}",
            path.display(),
            e
        )
    });

    assert!(
        contents.starts_with("---\n"),
        "template must start with `---` frontmatter fence, got: {:?}",
        &contents.chars().take(20).collect::<String>()
    );

    // Frontmatter must declare the template's identity and the label we filter on.
    assert!(
        contents.contains("name: Uninterpreted subprocess error"),
        "frontmatter missing `name:`"
    );
    assert!(
        contents.contains("labels: [\"error-interpretation\"]"),
        "frontmatter missing `labels:`"
    );

    // Required pre-fill sections — these mirror the help text in
    // `NeoError::SubprocessRaw` so the user can paste straight from the terminal.
    for section in [
        "What command did you run",
        "neo --version",
        "Operating system",
        "Full child output",
        "What were you trying to do",
    ] {
        assert!(
            contents.contains(section),
            "template missing required section: {:?}",
            section
        );
    }

    // The full-output section must reference the exact fence the error block uses,
    // so the user knows what to copy.
    assert!(
        contents.contains("--- full child output ---"),
        "template should reference the error's fence delimiter"
    );
}
