use super::*;
use std::os::unix::fs::PermissionsExt;

#[test]
fn fragmented_and_modified_keys_cannot_become_consent() {
    let now = Instant::now();
    for sequence in [
        b"\x1b[13;2u".as_slice(),
        b"\x1b[27;2;13~",
        b"\x1bOM",
        b"\xc3\xa9",
    ] {
        for split in 0..=sequence.len() {
            let mut input = Input::new();
            let mut keys = input.decode(&sequence[..split], now);
            keys.extend(input.decode(&sequence[split..], now));
            assert!(!keys.contains(&Key::Accept));
            assert_eq!(input.decode(b"\r", now), [Key::Accept]);
        }
    }
    let mut input = Input::new();
    let generation = input.generation();
    assert!(input.decode(b"\x1b", now).is_empty());
    assert_eq!(
        input.decode(b"", now + Duration::from_millis(101)),
        [Key::Back]
    );
    assert_eq!(input.generation(), generation);
}

#[test]
fn framed_paste_disposes_actions_and_overflow_cancels() {
    let now = Instant::now();
    let bytes = b"\x1b[200~d\rQ\r\x1b[201~";
    for split in 0..=bytes.len() {
        let mut input = Input::new();
        let mut keys = input.decode(&bytes[..split], now);
        keys.extend(input.decode(&bytes[split..], now));
        assert!(keys.is_empty());
        assert_eq!(input.decode(b"D", now), [Key::Details]);
    }
    let mut input = Input::new();
    input.decode(b"\x1b[200~", now);
    assert!(input.decode(&vec![b'x'; 4097], now).contains(&Key::Cancel));
    assert_eq!(input.decode(b"\r", now), [Key::Cancel]);
}

#[test]
fn bounded_frames_fail_before_growing_beyond_the_cap() {
    let mut output = BoundedFrame(Vec::new());
    output.write_all(&vec![b'x'; 256 * 1024]).unwrap();
    assert!(output.write_all(b"x").is_err());
    assert_eq!(output.0.len(), 256 * 1024);
    let mut buffer = Buffer::empty(Rect::new(0, 0, 1, 1));
    buffer[(0, 0)].set_symbol(&"x".repeat(256 * 1024));
    assert_eq!(encode(&buffer), Err(Failure::Output));
}

#[test]
fn safe_projection_rejects_control_text_and_bounds_details() {
    use super::super::observation::DisplayVersion;
    for version in [
        "1.2.3\x1b[2J",
        "01.2.3",
        "1.2.3+private",
        "1.2.3-rc.1",
        "/private/path",
    ] {
        assert!(DisplayVersion::parse(version).is_err());
    }
    let mut observation = PresentationObservation::new();
    for _ in 0..1000 {
        observation.activity(Activity::Review);
        observation.activity(Activity::Rechecking);
    }
    assert!(observation.details().count() <= 128);
    assert!(
        observation
            .details()
            .any(|detail| *detail == Detail::Truncated)
    );
    assert_eq!(DisplayVersion::parse("1.2.3").unwrap().to_string(), "1.2.3");
    let mut observation = PresentationObservation::new();
    let summary = super::super::ReviewSummary {
        operation: super::super::ReviewOperation::Recover,
        version: Some("1.2.3+private".into()),
        recovery: super::super::RecoveryPending::None,
        changes: Vec::new(),
    };
    assert!(observation.review(&summary).is_err());
}

#[test]
fn accepted_layout_uses_truthful_rows_and_small_size_falls_back() {
    let mut observation = PresentationObservation::new();
    observation.activity(Activity::PreparingPair);
    observation.rows[3] = Row::Working;
    observation.rows[4] = Row::Working;
    for size in [(80, 24), (42, 36)] {
        let frame = render(size, &observation, None, Page::Summary, 0, 0)
            .unwrap()
            .unwrap();
        let text = String::from_utf8(frame).unwrap();
        assert!(text.contains("Download neo"));
        assert!(text.contains("Download neoctl"));
        assert!(text.contains("Downloading neo and neoctl."));
        assert!(!text.contains("MiB"));
        assert!(!text.contains("DONE"));
    }
    assert!(
        render((40, 8), &observation, None, Page::Summary, 0, 0)
            .unwrap()
            .is_none()
    );
}

#[test]
#[ignore]
fn terminal_owner_fixture() {
    let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
    let state = root.join("state");
    std::fs::create_dir_all(&state).unwrap();
    std::fs::set_permissions(&state, std::fs::Permissions::from_mode(0o700)).unwrap();
    let mode = std::env::var("NEO_DASHBOARD_MODE").unwrap_or_default();
    PANIC.store(
        mode == "panic"
            || mode == "gated-panic"
            || mode == "cleanup-unwind"
            || mode == "worker-loss",
        std::sync::atomic::Ordering::Relaxed,
    );
    if mode == "signal-restore-primary" {
        run_signal_cleanup_fixture();
        return;
    }
    let stdin_flags = unsafe { libc::fcntl(0, libc::F_GETFL) };
    let original_stdio = [0, 1, 2].map(|fd| unsafe { libc::fcntl(fd, libc::F_GETFD) });
    let tty = Terminal::probe().unwrap();
    let selected = tty.is_some();
    let signals = Signals::new().unwrap();
    let adapter_stdio = [0, 1, 2].map(|fd| unsafe { libc::fcntl(fd, libc::F_GETFD) });
    assert_eq!(original_stdio, adapter_stdio);
    let _panic = SilentPanic::new();
    let layout = super::super::Layout::new(
        state,
        if mode == "signed-owner" {
            root.join("bin")
        } else {
            root.clone()
        },
    );
    let mut bridge = None;
    let mut owner = if mode == "gated-panic" || mode == "cleanup-unwind" || mode == "active-resize"
    {
        let (entered_send, entered_receive) = std::sync::mpsc::sync_channel(1);
        let (release_send, release_receive) = std::sync::mpsc::sync_channel(1);
        let ready: i32 = std::env::var("NEO_FIXTURE_GATE_READY")
            .unwrap()
            .parse()
            .unwrap();
        let release: i32 = std::env::var("NEO_FIXTURE_GATE_RELEASE")
            .unwrap()
            .parse()
            .unwrap();
        bridge = Some(std::thread::spawn(move || {
            entered_receive.recv().unwrap();
            let mut byte = 0u8;
            unsafe {
                libc::write(ready, b"G".as_ptr().cast(), 1);
                libc::read(release, (&mut byte as *mut u8).cast(), 1);
            }
            release_send.send(()).unwrap();
        }));
        super::super::coordinator::dashboard_gated_fixture(layout, entered_send, release_receive)
    } else if mode == "worker-loss" || mode == "private-output" {
        super::super::coordinator::dashboard_process_fixture(layout, mode == "worker-loss")
    } else if mode == "signed-owner" {
        let scenario: serde_json::Value =
            serde_json::from_slice(&std::fs::read(root.join("scenario.json")).unwrap()).unwrap();
        let version = scenario["version"].as_str().map(str::to_owned);
        let request = match scenario["operation"].as_str().unwrap() {
            "install" => Request::Install(version),
            "update" => Request::Update(version),
            "rollback" => Request::Rollback,
            _ => panic!("unsupported signed owner scenario"),
        };
        super::super::coordinator::dashboard_owned_fixture(layout, request)
    } else {
        Coordinator::new(
            Request::RecoverOnly,
            Executor::NativeInstaller,
            Some(layout),
        )
    };
    let mask_before = current_mask();
    let report = protected(
        &mut owner,
        tty,
        &signals,
        Options {
            yes: false,
            display: DisplayPreference::Still,
        },
    );
    let mode_after_protected = (mode == "small-layout-foreign").then(|| mode_snapshot(1));
    let mask_restored = current_mask() == mask_before;
    drop(owner);
    if let Some(mut child) = FOREGROUND_CHILD.lock().unwrap().take() {
        let mut mask = unsafe { std::mem::zeroed::<libc::sigset_t>() };
        let mut previous = mask;
        unsafe {
            libc::sigemptyset(&mut mask);
            libc::sigaddset(&mut mask, libc::SIGTTOU);
            assert_eq!(
                libc::pthread_sigmask(libc::SIG_BLOCK, &mask, &mut previous),
                0
            );
            assert_eq!(libc::tcsetpgrp(1, libc::getpgrp()), 0);
            assert_eq!(
                libc::pthread_sigmask(libc::SIG_SETMASK, &previous, std::ptr::null_mut()),
                0
            );
        }
        child.kill().unwrap();
        child.wait().unwrap();
    }
    if let Some(bridge) = bridge {
        bridge.join().unwrap();
    }
    let facts = report
        .observations
        .as_ref()
        .expect("owner facts survive terminal settlement");
    assert!(
        facts
            .events
            .windows(2)
            .all(|pair| pair[0].elapsed_ms <= pair[1].elapsed_ms)
    );
    assert!(facts.events.last().unwrap().elapsed_ms <= facts.elapsed_ms);
    let outcome = match report.operation {
        Ok(RunOutcome::Complete(_)) => "complete",
        Ok(RunOutcome::Unadmitted(ref value)) => match value.reason {
            super::super::UnadmittedReason::Declined => "declined",
            super::super::UnadmittedReason::ConsentRequired => "consent-required",
            super::super::UnadmittedReason::Cancelled => "cancelled",
            super::super::UnadmittedReason::ReviewChanged => "changed",
        },
        Ok(RunOutcome::Stopped(_)) => "stopped",
        Err(TransactionError::CleanupUnknown) => "cleanup-unknown",
        Err(_) => "failed",
    };
    std::fs::write(root.join("result.json"), serde_json::to_vec(&serde_json::json!({
        "dashboard": selected, "outcome": outcome,
        "presentation_failed": report.presentation.is_some(),
        "restored": report.presentation.is_none_or(|failure| failure.restored),
        "primary": report.presentation.map(|failure| match failure.primary { Failure::Panic => "panic", Failure::Input => "input", Failure::Output => "output", Failure::Restore => "restore", Failure::Setup => "setup", Failure::DisplayData => "display" }),
        "stdin_flags_before": stdin_flags,
        "stdin_flags_after": unsafe { libc::fcntl(0, libc::F_GETFL) },
        "adapter_stdio_preserved": original_stdio == adapter_stdio,
        "signal_mask_restored": mask_restored,
        "advances": ADVANCES.load(std::sync::atomic::Ordering::Relaxed),
        "mode_after_protected": mode_after_protected,
        "mode_after_cleanup": (mode == "small-layout-foreign").then(|| mode_snapshot(1)),
    })).unwrap()).unwrap();
    if mode == "worker-loss"
        || mode == "private-output"
        || mode == "cleanup-unwind"
        || mode == "signed-owner"
        || mode == "small-layout-foreign"
    {
        let exit = crate::format_report(report);
        std::fs::write(root.join("formatted-exit"), exit.code.to_string()).unwrap();
    }
    if let (Ok(ready), Ok(release)) = (
        std::env::var("NEO_FIXTURE_READY_FD"),
        std::env::var("NEO_FIXTURE_RELEASE_FD"),
    ) {
        let ready: i32 = ready.parse().unwrap();
        let release: i32 = release.parse().unwrap();
        let mut byte = 0u8;
        unsafe {
            libc::write(ready, b"R".as_ptr().cast(), 1);
            libc::read(release, (&mut byte as *mut u8).cast(), 1);
        }
    }
}

fn run_signal_cleanup_fixture() {
    let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
    let state = root.join("state");
    std::fs::create_dir_all(&state).unwrap();
    std::fs::set_permissions(&state, std::fs::Permissions::from_mode(0o700)).unwrap();
    PANIC.store(true, std::sync::atomic::Ordering::Relaxed);
    let mode_before = mode_snapshot(1);
    let mask_before = current_mask();
    let mut before = unsafe { std::mem::zeroed::<libc::sigaction>() };
    unsafe {
        libc::sigaction(libc::SIGWINCH, std::ptr::null(), &mut before);
    }
    let report = run(
        Request::RecoverOnly,
        Executor::NativeInstaller,
        Options {
            yes: false,
            display: DisplayPreference::Still,
        },
    );
    let mode_after = mode_snapshot(1);
    let mask_restored = current_mask() == mask_before;
    let mut after = unsafe { std::mem::zeroed::<libc::sigaction>() };
    unsafe {
        libc::sigaction(libc::SIGWINCH, std::ptr::null(), &mut after);
    }
    let expected_foreign = FOREIGN_ACTION.lock().unwrap().take().unwrap();
    let foreign_action_mask_preserved = (1..=128).all(|signal| unsafe {
        libc::sigismember(&after.sa_mask, signal)
            == libc::sigismember(&expected_foreign.sa_mask, signal)
    });
    let foreign_action_handler_preserved = after.sa_sigaction == expected_foreign.sa_sigaction;
    let foreign_action_flags_preserved = after.sa_flags == expected_foreign.sa_flags;
    let foreign_action_preserved = foreign_action_handler_preserved
        && foreign_action_flags_preserved
        && foreign_action_mask_preserved;
    let operation = match &report.operation {
        Ok(RunOutcome::Complete(_)) => "complete",
        Ok(RunOutcome::Unadmitted(value)) => match value.reason {
            super::super::UnadmittedReason::Declined => "declined",
            super::super::UnadmittedReason::ConsentRequired => "consent-required",
            super::super::UnadmittedReason::Cancelled => "cancelled",
            super::super::UnadmittedReason::ReviewChanged => "changed",
        },
        Ok(RunOutcome::Stopped(_)) => "stopped",
        Err(TransactionError::CleanupUnknown) => "cleanup-unknown",
        Err(_) => "failed",
    };
    let restored = report.presentation.is_none_or(|failure| failure.restored);
    let primary = report.presentation.map(|failure| match failure.primary {
        Failure::Panic => "panic",
        Failure::Input => "input",
        Failure::Output => "output",
        Failure::Restore => "restore",
        Failure::Setup => "setup",
        Failure::DisplayData => "display",
    });
    std::fs::write(
        root.join("result.json"),
        serde_json::to_vec(&serde_json::json!({
            "dashboard": true,
            "operation": operation,
            "cleanup": report.cleanup,
            "presentation_failed": report.presentation.is_some(),
            "primary": primary,
            "restored": restored,
            "mode_before": mode_before,
            "mode_after": mode_after,
            "foreign_action_preserved": foreign_action_preserved,
            "signal_action_before": before.sa_sigaction,
            "signal_action_after": after.sa_sigaction,
            "signal_action_expected": expected_foreign.sa_sigaction,
            "signal_action_flags_after": after.sa_flags,
            "signal_action_flags_expected": expected_foreign.sa_flags,
            "signal_action_handler_preserved": foreign_action_handler_preserved,
            "signal_action_flags_preserved": foreign_action_flags_preserved,
            "signal_action_mask_preserved": foreign_action_mask_preserved,
            "signal_mask_restored": mask_restored,
        }))
        .unwrap(),
    )
    .unwrap();
    let exit = crate::format_report(report);
    std::fs::write(root.join("formatted-exit"), exit.code.to_string()).unwrap();
    if let (Ok(ready), Ok(release)) = (
        std::env::var("NEO_FIXTURE_READY_FD"),
        std::env::var("NEO_FIXTURE_RELEASE_FD"),
    ) {
        let ready: i32 = ready.parse().unwrap();
        let release: i32 = release.parse().unwrap();
        let mut byte = 0u8;
        unsafe {
            libc::write(ready, b"R".as_ptr().cast(), 1);
            libc::read(release, (&mut byte as *mut u8).cast(), 1);
        }
    }
}

pub(super) fn run_owner_fixture(request: Request, executor: Executor) -> Coordinator {
    let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
    let state = root.join("state");
    std::fs::create_dir_all(&state).unwrap();
    std::fs::set_permissions(&state, std::fs::Permissions::from_mode(0o700)).unwrap();
    Coordinator::new(
        request,
        executor,
        Some(super::super::Layout::new(state, root)),
    )
}

pub(super) fn prepare_small_layout_fallback() {
    if !fault_mode("small-layout-foreign") {
        return;
    }
    use std::os::unix::process::CommandExt;
    let child = std::process::Command::new("/bin/sleep")
        .arg("30")
        .process_group(0)
        .spawn()
        .unwrap();
    let child_pid = child.id();
    let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
    std::fs::write(root.join("foreground-child-pid"), child_pid.to_string()).unwrap();
    let mut foreign = unsafe { std::mem::zeroed::<libc::termios>() };
    assert_eq!(unsafe { libc::tcgetattr(1, &mut foreign) }, 0);
    foreign.c_lflag ^= libc::ECHO;
    assert_eq!(unsafe { libc::tcsetattr(1, libc::TCSANOW, &foreign) }, 0);
    assert_eq!(unsafe { libc::tcgetattr(1, &mut foreign) }, 0);
    assert_eq!(unsafe { libc::tcsetpgrp(1, child_pid as i32) }, 0);
    std::fs::write(
        root.join("foreign-mode.json"),
        serde_json::to_vec(&serde_json::json!({
            "mode": mode_value(&foreign),
            "child_pid": child_pid,
            "child_pgid": unsafe { libc::getpgid(child_pid as i32) },
            "child_sid": unsafe { libc::getsid(child_pid as i32) },
            "foreground_pgid": unsafe { libc::tcgetpgrp(1) },
            "adapter_sid": unsafe { libc::getsid(0) },
        }))
        .unwrap(),
    )
    .unwrap();
    *FOREGROUND_CHILD.lock().unwrap() = Some(child);
}

fn mode_snapshot(fd: i32) -> serde_json::Value {
    let mut mode = unsafe { std::mem::zeroed::<libc::termios>() };
    assert_eq!(unsafe { libc::tcgetattr(fd, &mut mode) }, 0);
    mode_value(&mode)
}

fn mode_value(mode: &libc::termios) -> serde_json::Value {
    serde_json::json!([
        mode.c_iflag,
        mode.c_oflag,
        mode.c_cflag,
        mode.c_lflag,
        unsafe { libc::cfgetispeed(mode) },
        unsafe { libc::cfgetospeed(mode) },
        mode.c_cc.to_vec()
    ])
}

#[test]
fn signal_registration_retires_writers_and_supports_reentry() {
    let temporary = tempfile::tempdir().unwrap();
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .args([
            "--exact",
            "installation::dashboard::tests::signal_retirement_fixture",
            "--ignored",
            "--nocapture",
        ])
        .output()
        .unwrap();
    std::fs::write(temporary.path().join("stdout"), &output.stdout).unwrap();
    std::fs::write(temporary.path().join("stderr"), &output.stderr).unwrap();
    assert!(
        output.status.success(),
        "child exited {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(String::from_utf8_lossy(&output.stdout).contains("1 passed"));
}

#[test]
#[ignore]
fn signal_retirement_fixture() {
    let mut previous = unsafe { std::mem::zeroed::<libc::sigaction>() };
    unsafe {
        libc::sigaction(libc::SIGWINCH, std::ptr::null(), &mut previous);
    }
    let open_descriptors = || -> std::collections::BTreeSet<i32> {
        (3..256)
            .filter(|fd| unsafe { libc::fcntl(*fd, libc::F_GETFD) } >= 0)
            .collect()
    };
    for iteration in 0..10 {
        let before = open_descriptors();
        let signals = Signals::new().unwrap();
        let introduced: Vec<_> = open_descriptors().difference(&before).copied().collect();
        let (ready_send, ready_receive) = std::sync::mpsc::sync_channel(1);
        let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let thread_stop = stop.clone();
        let worker = std::thread::spawn(move || {
            unsafe {
                libc::raise(libc::SIGWINCH);
            }
            ready_send.send(()).unwrap();
            while !thread_stop.load(std::sync::atomic::Ordering::Acquire) {
                unsafe {
                    libc::raise(libc::SIGWINCH);
                }
                std::thread::yield_now();
            }
        });
        ready_receive.recv().unwrap();
        unsafe {
            libc::raise(libc::SIGINT);
        }
        assert!(signals.drain().0);
        drop(signals);
        let files: Vec<_> = (0..=introduced.last().copied().unwrap())
            .map(|_| tempfile::tempfile().unwrap())
            .collect();
        let reused: std::collections::BTreeSet<_> = files.iter().map(AsRawFd::as_raw_fd).collect();
        std::thread::yield_now();
        stop.store(true, std::sync::atomic::Ordering::Release);
        worker.join().unwrap();
        assert!(introduced.iter().all(|fd| reused.contains(fd)));
        assert!(files.iter().all(|file| file.metadata().unwrap().len() == 0));
        println!("retirement {iteration} reused {introduced:?} without writes");
    }
    let mut after = unsafe { std::mem::zeroed::<libc::sigaction>() };
    unsafe {
        libc::sigaction(libc::SIGWINCH, std::ptr::null(), &mut after);
    }
    assert_eq!(previous.sa_sigaction, after.sa_sigaction);
}

static PANIC: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
static FOREIGN_ACTION: std::sync::Mutex<Option<libc::sigaction>> = std::sync::Mutex::new(None);
pub(super) fn inject(bytes: &[u8], owner: &mut Coordinator) {
    if fault_mode("signal-restore-primary") && bytes.contains(&b'p') {
        let mut foreign = unsafe { std::mem::zeroed::<libc::sigaction>() };
        foreign.sa_sigaction = prior_b as *const () as usize;
        unsafe {
            libc::sigemptyset(&mut foreign.sa_mask);
            assert_eq!(
                libc::sigaction(libc::SIGWINCH, &foreign, std::ptr::null_mut()),
                0
            );
            let mut installed = std::mem::zeroed::<libc::sigaction>();
            assert_eq!(
                libc::sigaction(libc::SIGWINCH, std::ptr::null(), &mut installed),
                0
            );
            assert_eq!(installed.sa_sigaction, foreign.sa_sigaction);
            *FOREIGN_ACTION.lock().unwrap() = Some(installed);
        }
    }
    if (fault_mode("foreground-loss") || fault_mode("foreground-changed")) && bytes.contains(&b'f')
    {
        use std::os::unix::process::CommandExt;
        if fault_mode("foreground-changed") {
            let mut changed = unsafe { std::mem::zeroed::<libc::termios>() };
            assert_eq!(unsafe { libc::tcgetattr(1, &mut changed) }, 0);
            changed.c_lflag ^= libc::ECHO;
            assert_eq!(unsafe { libc::tcsetattr(1, libc::TCSANOW, &changed) }, 0);
            assert_eq!(unsafe { libc::tcgetattr(1, &mut changed) }, 0);
            let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
            std::fs::write(
                root.join("foreign-mode.json"),
                serde_json::to_vec(&serde_json::json!([
                    changed.c_iflag,
                    changed.c_oflag,
                    changed.c_cflag,
                    changed.c_lflag,
                    unsafe { libc::cfgetispeed(&changed) },
                    unsafe { libc::cfgetospeed(&changed) },
                    changed.c_cc.to_vec()
                ]))
                .unwrap(),
            )
            .unwrap();
        }
        let child = std::process::Command::new("/bin/sleep")
            .arg("30")
            .process_group(0)
            .spawn()
            .unwrap();
        assert_eq!(unsafe { libc::tcsetpgrp(1, child.id() as i32) }, 0);
        *FOREGROUND_CHILD.lock().unwrap() = Some(child);
        unsafe {
            libc::raise(libc::SIGWINCH);
        }
    }
    if fault_mode("worker-loss") && bytes.contains(&b'p') {
        let pid = owner.dashboard_kill_worker();
        let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
        std::fs::write(root.join("killed-worker-pid"), pid.to_string()).unwrap();
    }
    if PANIC.load(std::sync::atomic::Ordering::Relaxed) && bytes.contains(&b'p') {
        panic!("PRIVATE-DASHBOARD-PANIC-PAYLOAD");
    }
}

#[test]
fn display_flags_share_parsing_and_never_supply_consent() {
    for operation in ["install", "update", "rollback"] {
        for (flag, display) in [
            ("--plain", DisplayPreference::Plain),
            ("--still", DisplayPreference::Still),
        ] {
            let args = [operation, flag].map(std::ffi::OsString::from);
            let (_, options) = crate::parse_invocation(&args).unwrap();
            assert_eq!(options.display, display);
            assert!(!options.yes);
            let args = [operation, flag, "--yes"].map(std::ffi::OsString::from);
            assert!(crate::parse_invocation(&args).unwrap().1.yes);
        }
    }
    for args in [
        vec!["--plain", "install"],
        vec!["install", "--plain", "--still"],
        vec!["install", "--still", "--still"],
        vec!["install", "--plain", "--plain"],
    ] {
        assert!(
            crate::parse_invocation(&args.into_iter().map(Into::into).collect::<Vec<_>>()).is_err()
        );
    }
}

static PARTIAL_STAGE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
fn partial_mode() -> String {
    std::env::var("NEO_DASHBOARD_MODE").unwrap_or_default()
}
pub(super) fn write_limit(total: usize, page: Page, offered: bool) -> usize {
    let mode = partial_mode();
    if mode == "partial-first"
        || (mode == "partial-redraw" && page == Page::Details)
        || (mode == "partial-resize" && offered)
    {
        128
    } else {
        total
    }
}
fn checkpoint_byte(byte: u8) {
    let fd: i32 = std::env::var("NEO_FIXTURE_GATE_READY")
        .unwrap()
        .parse()
        .unwrap();
    unsafe {
        libc::write(fd, (&byte as *const u8).cast(), 1);
    }
}
pub(super) fn partial_checkpoint(page: Page, offered: bool, offset: usize, total: usize) {
    let mode = partial_mode();
    let target = (mode == "partial-resize" && page == Page::Summary && offered)
        || (mode == "partial-redraw" && page == Page::Details);
    if target
        && offset < total
        && PARTIAL_STAGE
            .compare_exchange(
                0,
                1,
                std::sync::atomic::Ordering::SeqCst,
                std::sync::atomic::Ordering::SeqCst,
            )
            .is_ok()
    {
        checkpoint_byte(b'G');
        unsafe {
            libc::raise(libc::SIGSTOP);
        }
    }
}
pub(super) fn frame_checkpoint(armed: bool) {
    static READY: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    if armed
        && std::env::var_os("NEO_CLARITY_READY_FD").is_some()
        && !READY.swap(true, std::sync::atomic::Ordering::SeqCst)
    {
        let fd: i32 = std::env::var("NEO_CLARITY_READY_FD")
            .unwrap()
            .parse()
            .unwrap();
        assert_eq!(unsafe { libc::write(fd, b"R".as_ptr().cast(), 1) }, 1);
    }
    if PARTIAL_STAGE
        .compare_exchange(
            1,
            2,
            std::sync::atomic::Ordering::SeqCst,
            std::sync::atomic::Ordering::SeqCst,
        )
        .is_ok()
    {
        checkpoint_byte(if armed { b'A' } else { b'C' });
    } else if PARTIAL_STAGE.load(std::sync::atomic::Ordering::SeqCst) == 2 {
        checkpoint_byte(b'N');
    }
}
extern "C" fn prior_a(_: libc::c_int) {}
extern "C" fn prior_b(_: libc::c_int) {}
#[test]
#[ignore]
fn changed_prior_signal_action_is_rejected() {
    let mut first = unsafe { std::mem::zeroed::<libc::sigaction>() };
    first.sa_sigaction = prior_a as *const () as usize;
    unsafe {
        libc::sigemptyset(&mut first.sa_mask);
        assert_eq!(
            libc::sigaction(libc::SIGWINCH, &first, std::ptr::null_mut()),
            0
        );
    }
    drop(Signals::new().unwrap());
    let mut second = first;
    second.sa_sigaction = prior_b as *const () as usize;
    unsafe {
        assert_eq!(
            libc::sigaction(libc::SIGWINCH, &second, std::ptr::null_mut()),
            0
        );
    }
    let registration = Signals::new();
    let rejected = registration.is_err();
    drop(registration);
    let mut after = unsafe { std::mem::zeroed::<libc::sigaction>() };
    unsafe {
        assert_eq!(
            libc::sigaction(libc::SIGWINCH, std::ptr::null(), &mut after),
            0
        );
    }
    assert_eq!(
        after.sa_sigaction, second.sa_sigaction,
        "changed caller action must be preserved"
    );
    assert!(
        rejected,
        "changed prior action must reject before terminal mutation or owner progress"
    );
    for changed_mask in [false, true] {
        let mut changed = first;
        if changed_mask {
            unsafe {
                libc::sigaddset(&mut changed.sa_mask, libc::SIGUSR1);
            }
        } else {
            changed.sa_flags = libc::SA_RESTART;
        }
        unsafe {
            assert_eq!(
                libc::sigaction(libc::SIGWINCH, &changed, std::ptr::null_mut()),
                0
            );
        }
        assert!(Signals::new().is_err(), "changed mask or flags must reject");
    }
    unsafe {
        assert_eq!(
            libc::sigaction(libc::SIGWINCH, &first, std::ptr::null_mut()),
            0
        );
    }
    drop(Signals::new().unwrap());
}

#[test]
fn expired_control_and_paste_framing_disable_the_session() {
    let now = Instant::now();
    for prefix in [
        b"\x1b[".as_slice(),
        b"\x1b[200",
        b"\x1b[123",
        b"\x1b[200~payload",
    ] {
        let mut input = Input::new();
        assert!(input.decode(prefix, now).is_empty());
        assert_eq!(
            input.decode(b"~\r", now + Duration::from_secs(3)),
            [Key::Cancel]
        );
        input.reset();
        assert_eq!(
            input.decode(b"\r", now + Duration::from_secs(4)),
            [Key::Cancel]
        );
    }
    for bytes in [
        vec![b'\r'; 33],
        [b"\x1b[".as_slice(), &vec![b'1'; 65], b"~\r"].concat(),
        b"\x1b[\x00\r".to_vec(),
    ] {
        let mut input = Input::new();
        assert_eq!(input.decode(&bytes, now), [Key::Cancel]);
        input.reset();
        assert_eq!(input.decode(b"\r", now), [Key::Cancel]);
    }
}

pub(super) fn fault_mode(mode: &str) -> bool {
    std::env::var("NEO_DASHBOARD_MODE").ok().as_deref() == Some(mode)
}

#[test]
fn invalid_readiness_descriptor_reports_failure() {
    let descriptor = tempfile::tempfile().unwrap();
    let fd = descriptor.as_raw_fd();
    drop(descriptor);
    let result = terminal::wait(
        &mut [libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        }],
        None,
    );
    assert_eq!(result.unwrap_err().kind(), std::io::ErrorKind::InvalidInput);
}

#[test]
#[ignore]
fn terminal_readiness_fixture() {
    let root = std::path::PathBuf::from(std::env::var_os("NEO_DASHBOARD_FIXTURE").unwrap());
    let mut tty = Terminal::probe().unwrap().unwrap();
    let flags = unsafe { libc::fcntl(0, libc::F_GETFL) };
    tty.activate().unwrap();
    checkpoint_byte(b'G');
    let mut descriptors = [libc::pollfd {
        fd: tty.descriptor(),
        events: libc::POLLIN,
        revents: 0,
    }];
    terminal::wait(&mut descriptors, None).unwrap();
    checkpoint_byte(b'W');
    assert_eq!(descriptors[0].revents, libc::POLLIN);
    assert_eq!(tty.read().unwrap(), b"d");
    assert!(tty.restore());
    std::fs::write(root.join("result.json"), serde_json::to_vec(&serde_json::json!({
        "dashboard": true, "outcome": "readiness", "presentation_failed": false,
        "restored": true, "primary": null, "stdin_flags_before": flags,
        "stdin_flags_after": unsafe { libc::fcntl(0, libc::F_GETFL) },
        "adapter_stdio_preserved": true, "advances": 0, "wait_revents": descriptors[0].revents,
        "sid": unsafe { libc::getsid(0) }, "tty_sid": unsafe { libc::tcgetsid(tty.descriptor()) },
        "pgid": unsafe { libc::getpgrp() }, "foreground": unsafe { libc::tcgetpgrp(tty.descriptor()) },
    })).unwrap()).unwrap();
    let ready: i32 = std::env::var("NEO_FIXTURE_READY_FD")
        .unwrap()
        .parse()
        .unwrap();
    let release: i32 = std::env::var("NEO_FIXTURE_RELEASE_FD")
        .unwrap()
        .parse()
        .unwrap();
    let mut byte = 0u8;
    unsafe {
        libc::write(ready, b"R".as_ptr().cast(), 1);
        libc::read(release, (&mut byte as *mut u8).cast(), 1);
    }
}

static FOREGROUND_CHILD: std::sync::Mutex<Option<std::process::Child>> =
    std::sync::Mutex::new(None);

#[test]
#[ignore]
fn unestablished_signal_registry_is_rejected() {
    let external = unsafe { signal_hook::low_level::register(libc::SIGWINCH, || {}) }.unwrap();
    let signal_numbers = [
        libc::SIGINT,
        libc::SIGTERM,
        libc::SIGHUP,
        libc::SIGTSTP,
        libc::SIGWINCH,
    ];
    let before = signal_numbers.map(|signal| {
        let mut action = unsafe { std::mem::zeroed::<libc::sigaction>() };
        assert_eq!(
            unsafe { libc::sigaction(signal, std::ptr::null(), &mut action) },
            0
        );
        action
    });
    assert!(Signals::new().is_err());
    for (signal, previous) in signal_numbers.into_iter().zip(before) {
        let mut after = unsafe { std::mem::zeroed::<libc::sigaction>() };
        assert_eq!(
            unsafe { libc::sigaction(signal, std::ptr::null(), &mut after) },
            0
        );
        assert_eq!(after.sa_sigaction, previous.sa_sigaction);
        assert_eq!(after.sa_flags, previous.sa_flags);
        for number in 1..=128 {
            assert_eq!(
                unsafe { libc::sigismember(&after.sa_mask, number) },
                unsafe { libc::sigismember(&previous.sa_mask, number) }
            );
        }
    }
    assert!(signal_hook::low_level::unregister(external));
}

fn current_mask() -> Vec<i32> {
    let mut mask = unsafe { std::mem::zeroed::<libc::sigset_t>() };
    assert_eq!(
        unsafe { libc::pthread_sigmask(libc::SIG_SETMASK, std::ptr::null(), &mut mask) },
        0
    );
    (1..=128)
        .map(|signal| unsafe { libc::sigismember(&mask, signal) })
        .collect()
}

#[test]
fn cache_review_at_eighty_columns_preserves_every_setting_across_pages() {
    let summary = super::super::ReviewSummary {
        operation: super::super::ReviewOperation::Install,
        version: None,
        recovery: super::super::RecoveryPending::None,
        changes: vec![super::super::ReviewedChange::CacheSetup {
            target: "/etc/nix/nix.conf",
            additions: (0..35).map(|i| format!("setting-{i} = value\n")).collect(),
        }],
    };
    let pages = review_pages((80, 24), &summary).unwrap();
    assert!(pages.len() > 1);
    assert!(pages.iter().all(|page| page.text.lines().count() <= 15));
    assert!(
        pages
            .last()
            .unwrap()
            .controls
            .contains("[I] Authorize and install")
    );
    let text = pages
        .into_iter()
        .map(|p| p.text)
        .collect::<Vec<_>>()
        .join("\n");
    for i in 0..35 {
        assert!(text.contains(&format!("setting-{i} = value")));
    }
}

#[test]
fn progress_shows_the_known_daemon_and_the_next_user_action() {
    let mut observation = PresentationObservation::new();
    observation.rows[0] = Row::Existing;
    observation.activity(Activity::PreparingReview);
    let frame = render((80, 24), &observation, None, Page::Summary, 0, 0)
        .unwrap()
        .unwrap();
    let text = String::from_utf8(frame).unwrap();
    assert!(text.contains("Nix is running."));
    assert!(text.contains("You'll review the changes before installation starts."));
    assert!(!text.contains("UNSEEN"));
}

#[test]
fn install_review_separates_system_settings_from_user_changes() {
    let mut summary = super::super::ReviewSummary {
        operation: super::super::ReviewOperation::Install,
        version: Some("0.17.1".into()),
        recovery: super::super::RecoveryPending::None,
        changes: Vec::new(),
    };
    let pages = review_pages((80, 24), &summary).unwrap();
    assert_eq!(pages.len(), 1);
    assert!(pages[0].text.contains("Install NeoHaskell 0.17.1?"));
    assert!(!pages[0].text.contains("sudo"));
    assert!(pages[0].controls.contains("[I] Install"));
    summary
        .changes
        .push(super::super::ReviewedChange::CacheSetup {
            target: "/etc/nix/nix.custom.conf",
            additions: "extra-trusted-public-keys = test-1:example=".into(),
        });
    for size in [(80, 24), (42, 24)] {
        let pages = review_pages(size, &summary).unwrap();
        let summary_page = pages
            .iter()
            .position(|p| p.controls.contains("Review Nix settings"))
            .unwrap();
        let overview = pages[..=summary_page]
            .iter()
            .map(|p| p.text.as_str())
            .collect::<Vec<_>>()
            .join(" ");
        assert!(overview.contains("signing keys"));
        assert!(!overview.contains("Enable Nix features"));
        assert!(!overview.contains("Add package sources"));
        assert!(
            pages
                .iter()
                .skip(summary_page + 1)
                .any(|p| p.text.contains("test-1:example="))
        );
        assert!(
            pages
                .last()
                .unwrap()
                .controls
                .starts_with("[I] Authorize and install")
        );
    }
}

#[cfg(all(test, unix))]
mod report_tests {
    use crate::{format_report, installation};

    const REPORT_MARKER: &str = "NEO_REPORT_DIAGNOSTIC ";

    #[test]
    fn operation_result_survives_display_cleanup_failure() {
        for mode in [
            "failure",
            "success",
            "two_failures",
            "cleanup_failure",
            "cleanup_success",
            "cleanup_unknown",
        ] {
            let output = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "installation::dashboard::tests::report_tests::report_fixture",
                    "--ignored",
                    "--nocapture",
                ])
                .env("NEO_REPORT_TEST", mode)
                .output()
                .unwrap();
            assert_eq!(output.status.code(), Some(1));
            let stderr = String::from_utf8(output.stderr).unwrap();
            let stdout = String::from_utf8(output.stdout).unwrap();
            let record = stdout
                .lines()
                .find_map(|line| line.strip_prefix(REPORT_MARKER))
                .unwrap_or_else(|| panic!("missing report record for {mode}: {stdout}"));
            let diagnostic: serde_json::Value = serde_json::from_str(record).unwrap();
            assert_eq!(
                diagnostic["operation"]["kind"],
                if mode == "success" || mode == "cleanup_success" {
                    "complete"
                } else {
                    "failed"
                }
            );
            if mode.starts_with("cleanup_") {
                assert!(stderr.contains("background work stopped"), "{stderr}");
                assert!(stderr.contains("signal handlers"), "{stderr}");
                assert!(!stderr.contains("could not fully restore"), "{stderr}");
            } else {
                assert_eq!(
                    stderr.matches("could not fully restore").count(),
                    1,
                    "{stderr}"
                );
            }
            if mode == "success" || mode == "cleanup_success" {
                assert!(stdout.contains("NeoHaskell 9.8.7 is active."), "{stdout}");
            } else if mode == "cleanup_unknown" {
                assert_eq!(
                    stderr.matches("background work stopped").count(),
                    1,
                    "{stderr}"
                );
            } else {
                assert!(stderr.contains("existing command"), "{stderr}");
                assert!(stderr.contains("/isolated/bin/neo"), "{stderr}");
                let secondary = if mode.starts_with("cleanup_") {
                    "background work stopped"
                } else {
                    "could not fully restore"
                };
                assert!(stderr.find("existing command").unwrap() < stderr.find(secondary).unwrap());
            }
            if mode == "two_failures" {
                assert!(stderr.contains("Terminal input failed"), "{stderr}");
            }
        }
    }

    #[test]
    #[ignore]
    fn report_fixture() {
        let mode = std::env::var("NEO_REPORT_TEST").unwrap();
        let operation = if mode == "success" || mode == "cleanup_success" {
            Ok(installation::RunOutcome::Complete(Some(
                installation::runtime::Outcome {
                    version: "9.8.7".into(),
                    open_new_shell: false,
                    has_previous: false,
                    previous_missing: false,
                },
            )))
        } else if mode == "cleanup_unknown" {
            Err(installation::TransactionError::CleanupUnknown)
        } else {
            Err(installation::TransactionError::CommandPathConflict {
                command: installation::ManagedCommand::Neo,
                path: "/isolated/bin/neo".into(),
            })
        };
        let report = installation::DriveReport {
            cleanup: if mode.starts_with("cleanup_") {
                vec![
                    installation::CleanupFailure::BackgroundWork,
                    installation::CleanupFailure::SignalHandlers,
                ]
            } else {
                Vec::new()
            },
            operation,
            presentation: (!mode.starts_with("cleanup_")).then_some(
                installation::dashboard::PresentationFailure {
                    primary: if mode == "two_failures" {
                        installation::PresentationFailureKind::Input
                    } else {
                        installation::PresentationFailureKind::Restore
                    },
                    restored: false,
                },
            ),
            observations: None,
        };
        let outcome = crate::diagnostics::DiagnosticOutcome::from_drive_report(
            crate::diagnostics::RequestRoute::Install,
            &report,
        );
        println!("\n{REPORT_MARKER}{}", serde_json::to_string(&outcome).unwrap());
        let exit = format_report(report);
        std::process::exit(exit.code);
    }
}
