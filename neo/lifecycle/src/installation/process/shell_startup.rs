use super::*;
use std::collections::BTreeMap;
use std::os::fd::FromRawFd;

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub(crate) enum ShellKind {
    Bash,
    Zsh,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub(crate) enum StartupMode {
    Login,
    Interactive,
    InteractiveLogin,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ShellStartupRequest {
    pub(crate) shell: ShellKind,
    pub(crate) mode: StartupMode,
    pub(crate) home: PathBuf,
    pub(crate) directory: PathBuf,
    pub(crate) environment: BTreeMap<String, String>,
    pub(crate) neo: PathBuf,
    pub(crate) neoctl: PathBuf,
    pub(crate) neo_version: String,
    pub(crate) neoctl_version: String,
}

impl std::fmt::Debug for ShellStartupRequest {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ShellStartupRequest")
            .field("shell", &self.shell)
            .field("mode", &self.mode)
            .finish_non_exhaustive()
    }
}

const RESULT_FD: libc::c_int = 61;

const BASH: &str = r#"
set +m
[ "$(builtin type -t neo)" = file ] && [ "$(builtin command -v neo)" = "$1" ] || exit 41
[ "$(builtin type -t neoctl)" = file ] && [ "$(builtin command -v neoctl)" = "$2" ] || exit 41
builtin printf 'qualified\n' >&61
neo --version >&61 || exit 42
neoctl --version >&61 || exit 42
"#;

const ZSH: &str = r#"
unsetopt monitor
[[ "$(builtin whence -w neo)" = 'neo: command' && "$(builtin whence -p neo)" = "$1" ]] || exit 41
[[ "$(builtin whence -w neoctl)" = 'neoctl: command' && "$(builtin whence -p neoctl)" = "$2" ]] || exit 41
builtin printf 'qualified\n' >&61
neo --version >&61 || exit 42
neoctl --version >&61 || exit 42
"#;

impl ShellStartupRequest {
    fn interactive(&self) -> bool {
        !matches!(self.mode, StartupMode::Login)
    }

    pub(super) fn command(&self) -> io::Result<Command> {
        for path in [&self.home, &self.directory, &self.neo, &self.neoctl] {
            if !path.is_absolute() || path.to_str().is_none_or(|s| s.contains('\0')) {
                return Err(invalid("shell context path"));
            }
        }
        for version in [&self.neo_version, &self.neoctl_version] {
            if version.is_empty()
                || version.len() > 128
                || version.bytes().any(|b| b.is_ascii_control())
            {
                return Err(invalid("shell expected version"));
            }
        }
        if self.environment.len() > 256
            || self.environment.iter().any(|(key, value)| {
                key.is_empty() || key.contains(['=', '\0']) || value.contains('\0')
            })
        {
            return Err(invalid("shell environment"));
        }
        let (executable, script) = match self.shell {
            ShellKind::Bash => ("/bin/bash", BASH),
            ShellKind::Zsh => ("/bin/zsh", ZSH),
        };
        let mut command = Command::new(executable);
        command
            .arg(match self.mode {
                StartupMode::Login => "-lc",
                StartupMode::Interactive => "-ic",
                StartupMode::InteractiveLogin => "-lic",
            })
            .arg(script)
            .arg(executable)
            .arg(&self.neo)
            .arg(&self.neoctl)
            .env_clear()
            .envs(&self.environment)
            .env("HOME", &self.home)
            .current_dir(&self.directory);
        validate_argv_budget(&command)?;
        Ok(command)
    }
}

fn high_descriptor(fd: i32) -> io::Result<OwnedFd> {
    let duplicate = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, RESULT_FD + 1) };
    if duplicate < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(duplicate) })
}

fn terminal() -> io::Result<(File, File)> {
    let mut master = -1;
    let mut slave = -1;
    if unsafe {
        libc::openpty(
            &mut master,
            &mut slave,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    } != 0
    {
        return Err(io::Error::last_os_error());
    }
    let master = unsafe { File::from_raw_fd(master) };
    let slave = unsafe { File::from_raw_fd(slave) };
    for fd in [master.as_raw_fd(), slave.as_raw_fd()] {
        if unsafe { libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) } == -1 {
            return Err(io::Error::last_os_error());
        }
    }
    nonblocking(master.as_raw_fd())?;
    Ok((master, slave))
}

pub(super) fn supervise(
    request: &ShellStartupRequest,
    mut command: Command,
    control: &UnixStream,
    timeout: Duration,
) -> OperationResult {
    let mut result = OperationResult {
        exit_code: None,
        stdout: Vec::new(),
        stderr: Vec::new(),
        failure: None,
    };
    let running = (|| -> io::Result<()> {
        let (mut protocol, write_end) = UnixStream::pair()?;
        protocol.set_nonblocking(true)?;
        let writer = high_descriptor(write_end.as_raw_fd())?;
        drop(write_end);
        let mut tty = if request.interactive() {
            Some(terminal()?)
        } else {
            None
        };
        if let Some((_, slave)) = &tty {
            command
                .stdin(Stdio::from(slave.try_clone()?))
                .stdout(Stdio::from(slave.try_clone()?))
                .stderr(Stdio::from(slave.try_clone()?));
        } else {
            command
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null());
        }
        let writer_fd = writer.as_raw_fd();
        let interactive = request.interactive();
        unsafe {
            command.pre_exec(move || {
                if libc::setsid() < 0 || libc::dup2(writer_fd, RESULT_FD) < 0 {
                    return Err(io::Error::last_os_error());
                }
                if interactive && libc::ioctl(0, libc::TIOCSCTTY as _, 0) < 0 {
                    return Err(io::Error::last_os_error());
                }
                Ok(())
            });
        }
        let child = command.spawn()?;
        drop(writer);
        let mut terminal_master = tty.take().map(|(master, _)| master);
        let group = child.id() as i32;
        let mut client = ClientGroup {
            child,
            group,
            status: None,
            quiescent: false,
        };
        let deadline = Instant::now() + timeout;
        let mut bytes = Vec::new();
        loop {
            if let Some(master) = &mut terminal_master {
                let mut discarded = [0u8; 4096];
                for _ in 0..16 {
                    match master.read(&mut discarded) {
                        Ok(0) => break,
                        Ok(_) => {}
                        Err(e)
                            if e.kind() == io::ErrorKind::WouldBlock
                                || e.raw_os_error() == Some(libc::EIO) =>
                        {
                            break;
                        }
                        Err(e) if e.kind() == io::ErrorKind::Interrupted => break,
                        Err(e) => return Err(e),
                    }
                }
            }
            drain(&mut protocol, &mut bytes, 512, "shell result limit")?;
            if disconnected(control)? {
                return Err(invalid("shell cancelled"));
            }
            if client.reap()? {
                if drain(&mut protocol, &mut bytes, 512, "shell result limit")? {
                    return Err(invalid("shell result channel remains open"));
                }
                let expected = format!(
                    "qualified\n{}\n{}\n",
                    request.neo_version, request.neoctl_version
                );
                if !client.status.is_some_and(|s| s.success()) || bytes != expected.as_bytes() {
                    return Err(invalid("shell verification failed"));
                }
                result.exit_code = Some(0);
                return Ok(());
            }
            if client.status.is_some() {
                return Err(invalid("shell descendants remain"));
            }
            if Instant::now() >= deadline {
                return Err(invalid("shell deadline"));
            }
            let mut descriptors = [
                libc::pollfd {
                    fd: control.as_raw_fd(),
                    events: libc::POLLIN,
                    revents: 0,
                },
                libc::pollfd {
                    fd: protocol.as_raw_fd(),
                    events: libc::POLLIN,
                    revents: 0,
                },
                libc::pollfd {
                    fd: terminal_master.as_ref().map_or(-1, |f| f.as_raw_fd()),
                    events: libc::POLLIN,
                    revents: 0,
                },
            ];
            if unsafe { libc::poll(descriptors.as_mut_ptr(), descriptors.len() as _, 20) } < 0
                && io::Error::last_os_error().kind() != io::ErrorKind::Interrupted
            {
                return Err(io::Error::last_os_error());
            }
        }
    })();
    if running.is_err() {
        result.failure = Some("fresh shell verification failed".into());
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::os::unix::fs::PermissionsExt;

    fn fixture(
        shell: ShellKind,
        mode: StartupMode,
        startup: &str,
    ) -> (tempfile::TempDir, ShellStartupRequest) {
        let temporary = tempfile::tempdir().unwrap();
        let home = fs::canonicalize(temporary.path()).unwrap();
        let bin = home.join("bin");
        fs::create_dir(&bin).unwrap();
        for tool in ["neo", "neoctl"] {
            let path = bin.join(tool);
            fs::write(&path, format!("#!/bin/sh\nprintf '{tool} 1.2.3\\n'\n")).unwrap();
            fs::set_permissions(path, fs::Permissions::from_mode(0o700)).unwrap();
        }
        let body = format!("export PATH=\"$HOME/bin:/usr/bin:/bin\"\n{startup}\n");
        for name in [".bash_profile", ".bashrc", ".zprofile", ".zshrc"] {
            fs::write(home.join(name), &body).unwrap();
        }
        let mut environment = BTreeMap::new();
        environment.insert("PATH".into(), "/usr/bin:/bin".into());
        environment.insert("ZDOTDIR".into(), home.to_str().unwrap().into());
        environment.insert("TERM".into(), "dumb".into());
        let request = ShellStartupRequest {
            shell,
            mode,
            home: home.clone(),
            directory: home,
            environment,
            neo: bin.join("neo"),
            neoctl: bin.join("neoctl"),
            neo_version: "neo 1.2.3".into(),
            neoctl_version: "neoctl 1.2.3".into(),
        };
        (temporary, request)
    }

    fn run(request: &ShellStartupRequest, timeout: Duration, cancel: bool) -> OperationResult {
        let (control, controller) = UnixStream::pair().unwrap();
        control.set_nonblocking(true).unwrap();
        let cancellation = if cancel {
            let controller = controller.try_clone().unwrap();
            Some(std::thread::spawn(move || {
                std::thread::sleep(Duration::from_millis(50));
                controller.shutdown(Shutdown::Both).unwrap();
            }))
        } else {
            None
        };
        let result = supervise(request, request.command().unwrap(), &control, timeout);
        if let Some(cancellation) = cancellation {
            cancellation.join().unwrap();
        }
        result
    }

    fn shells() -> Vec<ShellKind> {
        let mut shells = vec![ShellKind::Bash];
        if Path::new("/bin/zsh").exists() {
            shells.push(ShellKind::Zsh);
        }
        shells
    }

    #[test]
    fn qualified_lookup_and_pinned_versions_in_all_startup_modes() {
        for shell in shells() {
            for mode in [
                StartupMode::Login,
                StartupMode::Interactive,
                StartupMode::InteractiveLogin,
            ] {
                let (_temporary, request) = fixture(shell, mode, "");
                let result = run(&request, Duration::from_secs(3), false);
                assert_eq!(
                    result.exit_code,
                    Some(0),
                    "{shell:?} {mode:?} {:?}",
                    result.failure
                );
                assert!(result.stdout.is_empty() && result.stderr.is_empty());
            }
        }
    }

    #[test]
    fn interactive_startup_has_a_controlling_terminal_and_noise_is_discarded() {
        for shell in shells() {
            let (_temporary, request) = fixture(
                shell,
                StartupMode::Interactive,
                "test -t 0 && test -t 1 && test -t 2 && test -r /dev/tty || exit 90\nprintf 'PRIVATE-STARTUP-OUT\\n'\nprintf 'PRIVATE-STARTUP-ERR\\n' >&2",
            );
            let result = run(&request, Duration::from_secs(3), false);
            assert_eq!(result.exit_code, Some(0));
            assert!(result.stdout.is_empty() && result.stderr.is_empty());
        }
    }

    #[test]
    fn aliases_functions_absence_and_wrong_versions_fail_without_payloads() {
        for shell in shells() {
            for startup in [
                "alias neo='printf PRIVATE-ALIAS'",
                "neo() { printf PRIVATE-FUNCTION; }",
                "export PATH=/usr/bin:/bin",
                "alias neoctl='printf PRIVATE-ALIAS'",
            ] {
                let (_temporary, request) = fixture(shell, StartupMode::Interactive, startup);
                let result = run(&request, Duration::from_secs(3), false);
                assert!(result.failure.is_some());
                assert!(result.stdout.is_empty() && result.stderr.is_empty());
                assert!(!format!("{result:?}").contains("PRIVATE"));
            }
            let (_temporary, mut request) = fixture(shell, StartupMode::Interactive, "");
            request.neo_version = "neo 9.9.9".into();
            let result = run(&request, Duration::from_secs(3), false);
            assert!(result.failure.is_some());
            assert!(result.stdout.is_empty() && result.stderr.is_empty());
        }
    }

    #[test]
    fn startup_timeout_and_cancellation_reap_the_shell_group() {
        for shell in shells() {
            for cancel in [false, true] {
                let (_temporary, request) = fixture(
                    shell,
                    StartupMode::Interactive,
                    "printf '%s' $$ > \"$HOME/shell-pid\"\ntrap '' TERM\nwhile :; do :; done",
                );
                let result = run(&request, Duration::from_millis(100), cancel);
                assert!(result.failure.is_some());
                assert!(result.stdout.is_empty() && result.stderr.is_empty());
                if let Ok(pid) = fs::read_to_string(request.home.join("shell-pid")) {
                    let pid: i32 = pid.parse().unwrap();
                    assert_eq!(unsafe { libc::kill(-pid, 0) }, -1);
                    assert_eq!(io::Error::last_os_error().raw_os_error(), Some(libc::ESRCH));
                } else {
                    assert!(cancel);
                }
            }
        }
    }

    #[test]
    fn same_version_at_another_path_and_protocol_overflow_are_rejected() {
        for shell in shells() {
            let (_temporary, request) = fixture(
                shell,
                StartupMode::Interactive,
                "mkdir -p \"$HOME/other\"\ncp \"$HOME/bin/neo\" \"$HOME/other/neo\"\nexport PATH=\"$HOME/other:$PATH\"",
            );
            let result = run(&request, Duration::from_secs(3), false);
            assert!(result.failure.is_some());
            assert!(result.stdout.is_empty() && result.stderr.is_empty());
            let (_temporary, request) =
                fixture(shell, StartupMode::Interactive, "printf '%0600d' 0 >&61");
            let result = run(&request, Duration::from_secs(3), false);
            assert!(result.failure.is_some());
            assert!(result.stdout.is_empty() && result.stderr.is_empty());
        }
    }

    #[test]
    fn open_result_channel_cannot_complete_even_after_both_commands_resolve() {
        let (_temporary, request) = fixture(
            ShellKind::Bash,
            StartupMode::Interactive,
            "(trap '' HUP; sleep 1) </dev/null >/dev/null 2>&1 & disown",
        );
        let result = run(&request, Duration::from_secs(3), false);
        assert_eq!(result.exit_code, None);
        assert!(result.failure.is_some());
    }

    #[test]
    fn request_is_mutating_bounded_and_redacted() {
        let (_temporary, mut request) = fixture(ShellKind::Bash, StartupMode::Interactive, "");
        request
            .environment
            .insert("PRIVATE".into(), "PRIVATE-PAYLOAD".into());
        assert!(!format!("{request:?}").contains("PRIVATE"));
        assert!(WorkerRequest::ShellStartup(request.clone()).mutates());
        request.neo_version = "forged\nline".into();
        assert!(request.command().is_err());
        request.neo_version = "neo 1.2.3".into();
        request
            .environment
            .insert("OVERSIZE".into(), "x".repeat(32768));
        assert!(request.command().is_err());
    }
}
