#![cfg(unix)]

use std::fs;
use std::process::{Command, Stdio};

#[test]
fn installed_binary_records_lifecycle_routes_and_excludes_generic_cli() {
    let temporary = tempfile::tempdir().unwrap();
    let home = fs::canonicalize(temporary.path()).unwrap();
    let state = home.join("state");
    let directory = state.join("neohaskell-diagnostics");
    for (arguments, code, count) in [
        (&["--help"][..], 0, 0),
        (&["--version"][..], 0, 0),
        (&["PRIVATE-UNRELATED-COMMAND"][..], 2, 0),
        (&["update", "--help"][..], 0, 1),
        (&["rollback", "--help"][..], 0, 2),
        (&["update", "--PRIVATE-ARGUMENT"][..], 2, 3),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_neo"))
            .args(arguments)
            .env_clear()
            .env("HOME", &home)
            .env("XDG_STATE_HOME", &state)
            .env("PATH", &home)
            .stdin(Stdio::null())
            .output()
            .unwrap();
        assert_eq!(output.status.code(), Some(code), "{arguments:?}");
        let reports: Vec<_> = if directory.exists() {
            fs::read_dir(&directory)
                .unwrap()
                .map(|entry| entry.unwrap().path())
                .filter(|path| {
                    path.extension()
                        .is_some_and(|extension| extension == "json")
                })
                .collect()
        } else {
            Vec::new()
        };
        assert_eq!(reports.len(), count);
        for path in reports {
            let bytes = fs::read(path).unwrap();
            let text = String::from_utf8(bytes).unwrap();
            assert!(!text.contains("PRIVATE-ARGUMENT"));
            let value: serde_json::Value = serde_json::from_str(&text).unwrap();
            assert_eq!(value["report"]["invocation"]["executor"], "installed");
            assert_eq!(value["report"]["state"]["kind"], "finished");
        }
    }
    assert!(!state.join("neohaskell").exists());
}
