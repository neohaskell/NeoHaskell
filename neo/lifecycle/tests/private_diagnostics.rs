#![cfg(unix)]

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

struct UserRoot {
    _temporary: tempfile::TempDir,
    home: PathBuf,
    state: PathBuf,
}

impl UserRoot {
    fn new() -> Self {
        let temporary = tempfile::tempdir().unwrap();
        let root = fs::canonicalize(temporary.path()).unwrap();
        let home = root.join("home");
        let state = root.join("state");
        fs::create_dir(&home).unwrap();
        fs::create_dir(&state).unwrap();
        Self {
            _temporary: temporary,
            home,
            state,
        }
    }

    fn command(&self, arguments: &[&str]) -> Command {
        let mut command = Command::new(env!("CARGO_BIN_EXE_neo-installer"));
        command
            .args(arguments)
            .env_clear()
            .env("HOME", &self.home)
            .env("XDG_STATE_HOME", &self.state)
            .env("PATH", &self.home)
            .stdin(Stdio::null());
        command
    }

    fn run(&self, arguments: &[&str]) -> Output {
        self.command(arguments).output().unwrap()
    }

    fn reports(&self) -> Vec<PathBuf> {
        let directory = self.state.join("neohaskell-diagnostics");
        if !directory.exists() {
            return Vec::new();
        }
        fs::read_dir(directory)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| {
                path.extension()
                    .is_some_and(|extension| extension == "json")
            })
            .collect()
    }
}

fn report(path: &Path) -> serde_json::Value {
    let metadata = fs::metadata(path).unwrap();
    assert_eq!(metadata.mode() & 0o7777, 0o600);
    assert_eq!(metadata.nlink(), 1);
    assert_eq!(metadata.uid(), unsafe { libc::geteuid() });
    assert!((1024..=16384).contains(&metadata.len()));
    serde_json::from_slice(&fs::read(path).unwrap()).unwrap()
}

#[test]
fn public_native_routes_publish_one_private_finished_record_each() {
    let user = UserRoot::new();
    for (index, (arguments, code)) in [
        (&[][..], 0),
        (&["--help"][..], 0),
        (&["--version"][..], 0),
        (&["--PRIVATE-ARGV-MARKER"][..], 2),
        (&["install", "--help"][..], 0),
        (&["rollback", "--version", "PRIVATE-VERSION-MARKER"][..], 2),
    ]
    .into_iter()
    .enumerate()
    {
        let output = user.run(arguments);
        assert_eq!(output.status.code(), Some(code));
        let paths = user.reports();
        assert_eq!(paths.len(), index + 1);
        for path in &paths {
            let value = report(path);
            let serialized = value.to_string();
            assert!(!serialized.contains("PRIVATE-ARGV-MARKER"));
            assert!(!serialized.contains("PRIVATE-VERSION-MARKER"));
            assert!(!serialized.contains(user.home.to_str().unwrap()));
            assert!(!serialized.contains(user.state.to_str().unwrap()));
            assert_eq!(value["report"]["state"]["kind"], "finished");
            let build = &value["report"]["build"];
            assert_eq!(build["target"], env!("NEO_BUILD_TARGET"));
            assert!(build["product_version"].as_str().is_some());
            assert!(matches!(
                build["source_revision"]["kind"].as_str(),
                Some("development" | "revision")
            ));
        }
        if code != 0 {
            let stderr = String::from_utf8(output.stderr).unwrap();
            assert!(
                paths
                    .iter()
                    .any(|path| stderr.contains(path.to_str().unwrap()))
            );
        }
    }
    assert!(!user.state.join("neohaskell").exists());
    let directory = user.state.join("neohaskell-diagnostics");
    assert_eq!(fs::metadata(&directory).unwrap().mode() & 0o7777, 0o700);
    assert_eq!(fs::metadata(directory.join("lock")).unwrap().len(), 0);
}

#[test]
fn failure_prints_the_literal_private_report_location() {
    let mut user = UserRoot::new();
    user.state = user.state.join("camión's \\ notes");
    fs::create_dir(&user.state).unwrap();
    let output = user.run(&["--unknown"]);
    assert_eq!(output.status.code(), Some(2));
    let paths = user.reports();
    assert_eq!(paths.len(), 1);
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains(paths[0].to_str().unwrap())
    );
}

#[test]
fn owner_masking_umask_never_poisoned_the_private_store() {
    let user = UserRoot::new();
    let mut command = user.command(&["--unknown"]);
    unsafe {
        command.pre_exec(|| {
            libc::umask(0o777);
            Ok(())
        });
    }
    let output = command.output().unwrap();
    assert_eq!(output.status.code(), Some(2));
    assert!(!user.state.join("neohaskell-diagnostics").exists());
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .contains("report unavailable")
    );
    assert_eq!(user.run(&["--help"]).status.code(), Some(0));
    let paths = user.reports();
    assert_eq!(paths.len(), 1);
    report(&paths[0]);
}

#[test]
fn exact_internal_route_does_not_create_public_state() {
    let user = UserRoot::new();
    let output = user.run(&["--internal-operation-worker"]);
    assert_eq!(output.status.code(), Some(125));
    assert!(output.stdout.is_empty());
    assert!(output.stderr.is_empty());
    let client = user.run(&["--internal-release-client", "not-json"]);
    assert_eq!(client.status.code(), Some(42));
    assert!(client.stdout.is_empty());
    assert!(client.stderr.is_empty());
    assert!(!user.state.join("neohaskell-diagnostics").exists());
}

#[test]
fn missing_home_preserves_exit_and_reports_unavailable() {
    let user = UserRoot::new();
    let output = user
        .command(&["--unknown"])
        .env_remove("HOME")
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(2));
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .to_ascii_lowercase()
            .contains("report unavailable")
    );
    assert!(!user.state.join("neohaskell-diagnostics").exists());
    let output = user
        .command(&["--help"])
        .env_remove("HOME")
        .output()
        .unwrap();
    assert!(output.status.success());
    assert!(output.stderr.is_empty());
}

#[test]
fn unsafe_store_keeps_public_behavior_without_touching_target() {
    let user = UserRoot::new();
    let target = user.home.join("foreign");
    fs::create_dir(&target).unwrap();
    std::os::unix::fs::symlink(&target, user.state.join("neohaskell-diagnostics")).unwrap();
    let output = user.run(&["--unknown"]);
    assert_eq!(output.status.code(), Some(2));
    assert!(
        String::from_utf8(output.stderr)
            .unwrap()
            .to_ascii_lowercase()
            .contains("report unavailable")
    );
    assert_eq!(fs::read_dir(target).unwrap().count(), 0);
}

#[test]
fn native_and_installed_routes_share_the_private_store() {
    if let Ok(operation) = std::env::var("NEO_DIAGNOSTIC_TEST_OPERATION") {
        assert_eq!(
            neo_lifecycle::dispatch_installed(&[operation.into(), "--help".into()])
                .unwrap()
                .code(),
            0
        );
        return;
    }
    let user = UserRoot::new();
    assert!(user.run(&["--help"]).status.success());
    for operation in ["update", "rollback"] {
        let output = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "native_and_installed_routes_share_the_private_store",
                "--nocapture",
            ])
            .env_clear()
            .env("HOME", &user.home)
            .env("XDG_STATE_HOME", &user.state)
            .env("NEO_DIAGNOSTIC_TEST_OPERATION", operation)
            .output()
            .unwrap();
        assert!(output.status.success(), "{output:?}");
    }
    assert_eq!(user.reports().len(), 3);
    for path in user.reports() {
        report(&path);
    }
}

#[test]
fn concurrent_public_processes_leave_only_parseable_reports() {
    let user = UserRoot::new();
    let mut children = Vec::new();
    for _ in 0..8 {
        children.push(
            user.command(&["--help"])
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
                .unwrap(),
        );
    }
    for mut child in children {
        assert!(child.wait().unwrap().success());
    }
    let reports = user.reports();
    assert!(!reports.is_empty());
    assert!(reports.len() <= 8);
    for path in reports {
        report(&path);
    }
}

#[test]
fn actual_parsed_rollback_is_recorded_without_command_input() {
    let user = UserRoot::new();
    let occupation = user.state.join("neohaskell");
    fs::write(&occupation, b"foreign state").unwrap();
    let output = user.run(&["update", "--rollback", "--plain"]);
    assert!(!output.status.success());
    let paths = user.reports();
    assert_eq!(paths.len(), 1);
    let value = report(&paths[0]);
    assert_eq!(
        value["report"]["invocation"]["route"]["operation"],
        "update"
    );
    assert_eq!(value["report"]["state"]["outcome"]["request"], "rollback");
    assert_eq!(
        value["report"]["state"]["outcome"]["operation"]["error"]["kind"],
        "unsafe_path"
    );
    assert_eq!(fs::read(occupation).unwrap(), b"foreign state");
}
