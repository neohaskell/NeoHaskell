use crate::output::OutputMode;
use crate::errors::NeoError;
use std::process::Stdio;
use tokio::process::Command;
use std::path::PathBuf;
use walkdir::WalkDir;

pub struct HurlResult {
    #[allow(dead_code)]
    pub file: PathBuf,
    pub success: bool,
    pub duration: std::time::Duration,
}

fn sanitized_ci_output(bytes: &[u8]) -> String {
    const MAX_LINES: usize = 200;
    const MAX_CHARS: usize = 16_000;
    const SENSITIVE: &[&str] = &[
        "authorization", "cookie", "password", "passwd", "token=", "api_key",
        "apikey", "secret", "credential",
    ];

    let text = String::from_utf8_lossy(bytes);
    let mut lines = text.lines().rev().take(MAX_LINES).collect::<Vec<_>>();
    lines.reverse();
    let redacted = lines
        .into_iter()
        .map(|line| {
            let lower = line.to_ascii_lowercase();
            if SENSITIVE.iter().any(|needle| lower.contains(needle)) {
                "[REDACTED]"
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n");
    if redacted.chars().count() <= MAX_CHARS {
        redacted
    } else {
        let tail = redacted.chars().rev().take(MAX_CHARS).collect::<Vec<_>>();
        format!("[output truncated]\n{}", tail.into_iter().rev().collect::<String>())
    }
}

pub async fn discover_tests(base_path: Option<&std::path::Path>) -> miette::Result<Vec<PathBuf>> {
    let mut tests = Vec::new();
    let tests_dir = if let Some(base) = base_path {
        base.join("tests")
    } else {
        PathBuf::from("tests")
    };
    
    if !tests_dir.exists() {
        return Ok(tests);
    }

    for entry in WalkDir::new(tests_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "hurl"))
    {
        tests.push(entry.into_path());
    }
    
    Ok(tests)
}

pub async fn run_test(path: &PathBuf, output_mode: &mut OutputMode) -> miette::Result<HurlResult> {
    let start = std::time::Instant::now();
    
    if output_mode.is_ci() {
        println!("[info] Running Hurl test: {:?}", path);
    }

    let child = Command::new("nix")
        .args(["develop", "--command", "hurl", "--test"])
        .arg(path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| NeoError::SubprocessFailed {
            operation: format!("spawning `nix develop --command hurl --test {}`", path.display()),
            cause: format!("could not spawn child process: {}", e),
            fix: "Ensure `nix` is installed and `hurl` is available inside `nix develop` (it is in this repo's flake). Re-run from the project root.".to_string(),
        })?;

    let output = child.wait_with_output().await
        .map_err(|e| NeoError::SubprocessFailed {
            operation: format!("waiting on `hurl --test {}`", path.display()),
            cause: format!("could not reap child process: {}", e),
            fix: "Re-run. If reproducible, your shell may be out of file descriptors (`ulimit -n`).".to_string(),
        })?;

    let duration = start.elapsed();
    if output_mode.is_ci() && !output.status.success() {
        let stdout = sanitized_ci_output(&output.stdout);
        let stderr = sanitized_ci_output(&output.stderr);
        if !stdout.trim().is_empty() {
            eprintln!("[fail] hurl stdout for {}:\n{}", path.display(), stdout.trim_end());
        }
        if !stderr.trim().is_empty() {
            eprintln!("[fail] hurl stderr for {}:\n{}", path.display(), stderr.trim_end());
        }
    }
    
    Ok(HurlResult {
        file: path.clone(),
        success: output.status.success(),
        duration,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_hurl_discovery() {
        let dir = tempdir().unwrap();
        let tests_dir = dir.path().join("tests");
        fs::create_dir_all(&tests_dir).unwrap();
        fs::write(tests_dir.join("test1.hurl"), "").unwrap();
        fs::write(tests_dir.join("test2.hurl"), "").unwrap();
        fs::write(tests_dir.join("not_hurl.txt"), "").unwrap();

        let tests = discover_tests(Some(dir.path())).await.unwrap();
        
        assert_eq!(tests.len(), 2);
        let names: Vec<String> = tests.iter().map(|p| p.file_name().unwrap().to_str().unwrap().to_string()).collect();
        assert!(names.contains(&"test1.hurl".to_string()));
        assert!(names.contains(&"test2.hurl".to_string()));
    }

    #[tokio::test]
    async fn test_hurl_discovery_empty() {
        let dir = tempdir().unwrap();
        let tests = discover_tests(Some(dir.path())).await.unwrap();
        assert!(tests.is_empty());
    }

    #[test]
    fn ci_output_redacts_sensitive_lines_and_keeps_diagnostics() {
        let output = sanitized_ci_output(
            b"assertion failed\nAuthorization: Bearer private\npassword=hunter2\nstatus 500\n",
        );
        assert!(output.contains("assertion failed"));
        assert!(output.contains("status 500"));
        assert_eq!(output.matches("[REDACTED]").count(), 2);
        assert!(!output.contains("private"));
        assert!(!output.contains("hunter2"));
    }
}
