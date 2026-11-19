mod candidate;
#[cfg(unix)]
mod diagnostics;
mod identity;
#[cfg(unix)]
mod installation;
mod language;

use std::sync::OnceLock;

static ENGLISH: OnceLock<language::English> = OnceLock::new();

pub struct Exit {
    code: i32,
}

impl Exit {
    pub fn code(self) -> i32 {
        self.code
    }
}

pub fn dispatch_internal(arguments: &[std::ffi::OsString]) -> Option<Exit> {
    #[cfg(target_os = "linux")]
    if let Some(code) = installation::nix_owner::dispatch_native_policy(arguments) {
        return Some(Exit { code });
    }
    #[cfg(target_os = "linux")]
    if arguments
        == [std::ffi::OsString::from(
            installation::nix_owner::POLICY_OBSERVE_ARGUMENT,
        )]
    {
        return Some(Exit {
            code: installation::nix_owner::run_policy_observer(),
        });
    }
    dispatch_installed_internal(arguments)
}

fn dispatch_installed_internal(arguments: &[std::ffi::OsString]) -> Option<Exit> {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    if let Some(code) = installation::nix_owner::darwin::dispatch_administrator(arguments) {
        return Some(Exit { code });
    }
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    if matches!(arguments, [arg] if arg == installation::nix_owner::darwin::OBSERVE_ARGUMENT) {
        return Some(Exit {
            code: installation::nix_owner::darwin::run_observer(),
        });
    }
    #[cfg(target_os = "linux")]
    if let Some(code) = installation::nix_owner::dispatch_caller_access(arguments) {
        return Some(Exit { code });
    }
    #[cfg(target_os = "linux")]
    if let Some(code) = installation::nix_owner::dispatch_existing_policy(arguments) {
        return Some(Exit { code });
    }
    #[cfg(unix)]
    if arguments
        == [std::ffi::OsString::from(
            installation::process::WORKER_ARGUMENT,
        )]
    {
        return Some(Exit {
            code: installation::process::run_worker(),
        });
    }
    #[cfg(unix)]
    if let [argument, request] = arguments {
        if argument == installation::release::CLIENT_ARGUMENT {
            return Some(Exit {
                code: installation::release::run_client(request),
            });
        }
    }
    None
}

#[cfg(all(test, target_os = "linux"))]
#[test]
fn installed_cli_dispatches_read_only_policy_helpers() {
    for argument in [
        "--neo-policy-launch",
        "--neo-policy-observe",
        "--neo-policy-observe-peer",
        "--neo-policy-publish-peer",
        "--neo-policy-inspect-peer",
    ] {
        assert!(dispatch_installed_internal(&[argument.into()]).is_none());
    }
    let invalid = [
        "--neo-policy-existing-peer".into(),
        "invalid-challenge".into(),
    ];
    assert_eq!(dispatch_installed_internal(&invalid).unwrap().code(), 1);
    assert_eq!(dispatch_internal(&invalid).unwrap().code(), 1);
}

pub fn installed_help() -> &'static str {
    &english().installed_lifecycle_help
}

pub fn dispatch_installed(arguments: &[std::ffi::OsString]) -> Option<Exit> {
    if let Some(exit) = dispatch_installed_internal(arguments) {
        return Some(exit);
    }
    #[cfg(unix)]
    if arguments.first().is_some_and(|argument| {
        argument == "update" || argument == "rollback" || argument == "uninstall"
    }) {
        let route = if arguments[0] == "update" {
            diagnostics::RequestRoute::Update
        } else if arguments[0] == "uninstall" {
            diagnostics::RequestRoute::Uninstall
        } else {
            diagnostics::RequestRoute::Rollback
        };
        return Some(diagnostics::record_public(
            diagnostics::Invocation::Installed(route),
            || {
                if arguments.len() == 2 && (arguments[1] == "--help" || arguments[1] == "-h") {
                    println!("{}", installed_help());
                    diagnostics::InvocationResult {
                        exit: Exit { code: 0 },
                        outcome: diagnostics::DiagnosticOutcome::HelpDisplayed,
                    }
                } else {
                    run_request(arguments, installation::Executor::InstalledNeo)
                }
            },
        ));
    }
    None
}

#[cfg(unix)]
fn parse_invocation(
    arguments: &[std::ffi::OsString],
) -> Result<(installation::Request, installation::Options), ()> {
    if arguments
        .first()
        .is_none_or(|argument| argument.to_str().is_none_or(|value| value.starts_with('-')))
    {
        return Err(());
    }
    let mut consent = false;
    let mut display = installation::DisplayPreference::Automatic;
    let mut filtered = Vec::new();
    for argument in arguments {
        if argument == "--yes" {
            if consent {
                return Err(());
            }
            consent = true;
        } else if argument == "--plain" || argument == "--still" {
            if display != installation::DisplayPreference::Automatic {
                return Err(());
            }
            display = if argument == "--plain" {
                installation::DisplayPreference::Plain
            } else {
                installation::DisplayPreference::Still
            };
        } else {
            filtered.push(argument.clone());
        }
    }
    let request = match filtered.as_slice() {
        [operation] if operation == "install" => installation::Request::Install(None),
        [operation] if operation == "update" => installation::Request::Update(None),
        [operation, option] if operation == "update" && option == "--rollback" => {
            installation::Request::Rollback
        }
        [operation] if operation == "rollback" => installation::Request::Rollback,
        [operation] if operation == "uninstall" => installation::Request::Uninstall,
        [operation, option, version]
            if option == "--version" && (operation == "install" || operation == "update") =>
        {
            let Some(version) = version.to_str().filter(|version| {
                semver::Version::parse(version).is_ok_and(|parsed| {
                    parsed.pre.is_empty()
                        && parsed.build.is_empty()
                        && parsed.to_string() == *version
                })
            }) else {
                return Err(());
            };
            if operation == "install" {
                installation::Request::Install(Some(version.into()))
            } else {
                installation::Request::Update(Some(version.into()))
            }
        }
        _ => {
            return Err(());
        }
    };
    Ok((
        request,
        installation::Options {
            yes: consent,
            display,
        },
    ))
}

#[cfg(unix)]
fn parse_execution(
    arguments: &[std::ffi::OsString],
    executor: installation::Executor,
) -> Result<
    (
        installation::Request,
        installation::Options,
        installation::Executor,
    ),
    (),
> {
    let mut filtered = Vec::new();
    let mut locator = None;
    let mut arguments = arguments.iter();
    while let Some(argument) = arguments.next() {
        if argument == "--candidate-locator" {
            if locator.is_some() || !matches!(executor, installation::Executor::NativeInstaller) {
                return Err(());
            }
            let value = arguments
                .next()
                .and_then(|value| value.to_str())
                .ok_or(())?;
            locator = Some(installation::CandidateLocator::parse(value)?);
        } else {
            filtered.push(argument.clone());
        }
    }
    let (request, options) = parse_invocation(&filtered)?;
    let executor = match locator {
        Some(locator) if matches!(request, installation::Request::Install(_)) => {
            installation::Executor::Bootstrap(locator)
        }
        Some(_) => return Err(()),
        None => executor,
    };
    Ok((request, options, executor))
}

#[cfg(unix)]
fn run_request(
    arguments: &[std::ffi::OsString],
    executor: installation::Executor,
) -> diagnostics::InvocationResult {
    let english = english();
    if arguments.len() == 2 && (arguments[1] == "--help" || arguments[1] == "-h") {
        println!("{}", english.help);
        return diagnostics::InvocationResult {
            exit: Exit { code: 0 },
            outcome: diagnostics::DiagnosticOutcome::HelpDisplayed,
        };
    }
    let (request, consent, executor) = match parse_execution(arguments, executor) {
        Ok(invocation) => invocation,
        Err(()) => {
            eprintln!("{}", english.invalid_request);
            return diagnostics::InvocationResult {
                exit: Exit { code: 2 },
                outcome: diagnostics::DiagnosticOutcome::InvalidRequest,
            };
        }
    };
    let request_kind = match &request {
        installation::Request::Install(_) => diagnostics::RequestRoute::Install,
        installation::Request::Update(_) => diagnostics::RequestRoute::Update,
        installation::Request::Rollback => diagnostics::RequestRoute::Rollback,
        installation::Request::Uninstall => diagnostics::RequestRoute::Uninstall,
        installation::Request::RecoverOnly => unreachable!("public parser excludes recovery-only"),
    };
    let uninstall = matches!(request, installation::Request::Uninstall);
    let report = installation::run(request, executor, consent);
    if uninstall {
        match &report.operation {
            Ok(installation::RunOutcome::Complete(_)) => println!("{}", english.uninstall_complete),
            Err(_) | Ok(installation::RunOutcome::Stopped(_)) => {
                eprintln!("{}", english.uninstall_pending)
            }
            _ => {}
        }
    }
    let outcome = diagnostics::DiagnosticOutcome::from_drive_report(request_kind, &report);
    diagnostics::InvocationResult {
        exit: format_report(report),
        outcome,
    }
}

#[cfg(unix)]
fn format_report(report: installation::DriveReport) -> Exit {
    let english = english();
    if let Some(failure) = report.presentation {
        eprintln!(
            "{}",
            english.dashboard.presentation_errors[failure.primary as usize]
        );
        if !failure.restored {
            eprintln!("{}", english.dashboard.restoration_failed);
        }
        if matches!(
            report.operation,
            Err(installation::TransactionError::CleanupUnknown)
        ) {
            eprintln!("{}", english.cleanup_unknown);
        }
        return Exit { code: 1 };
    }
    format_result(report.operation)
}

#[cfg(unix)]
fn format_result(result: Result<installation::RunOutcome, installation::TransactionError>) -> Exit {
    let english = english();
    match result {
        Ok(installation::RunOutcome::Complete(Some(outcome))) => {
            println!(
                "{}",
                english
                    .operation_complete
                    .replace("{version}", &outcome.version)
            );
            if outcome.has_previous {
                println!(
                    "{}",
                    if outcome.previous_missing {
                        &english.previous_missing
                    } else {
                        &english.previous_unchecked
                    }
                );
            }
            Exit { code: 0 }
        }
        Ok(installation::RunOutcome::Complete(None)) => Exit { code: 0 },
        Ok(installation::RunOutcome::Unadmitted(stopped)) => {
            use installation::UnadmittedReason;
            let (message, code) = match stopped.reason {
                UnadmittedReason::ConsentRequired => (&english.consent_required, 2),
                UnadmittedReason::Declined => (&english.review_declined, 2),
                UnadmittedReason::Cancelled => (&english.operation_not_started, 130),
                UnadmittedReason::ReviewChanged => (&english.review_changed, 2),
            };
            eprintln!("{message}");
            if !matches!(stopped.recovery, installation::RecoveryPending::None) {
                eprintln!("{}", english.recovery_unadmitted);
            }
            Exit { code }
        }
        Ok(installation::RunOutcome::Stopped(stopped)) => {
            match stopped.observation {
                installation::StopObservation::NotRequired => {
                    eprintln!("{}", english.operation_not_started)
                }
                installation::StopObservation::ObservedNoPair => {
                    eprintln!("{}", english.operation_stopped_no_pair)
                }
                installation::StopObservation::ObservedPair(pair) => eprintln!(
                    "{}",
                    english
                        .operation_stopped_pair
                        .replace("{version}", &pair.version)
                ),
                #[cfg(any(target_os = "linux", all(target_os = "macos", target_arch = "aarch64")))]
                installation::StopObservation::PolicyRecoveryInterrupted => {
                    eprintln!("{}", english.nix_policy_recovery_pending)
                }
            }
            Exit { code: 130 }
        }
        Err(installation::TransactionError::ShellRecoveryCompleted) => {
            eprintln!("{}", english.shell_recovery_completed);
            Exit { code: 2 }
        }
        Err(installation::TransactionError::ShellUnsupported) => {
            eprintln!("{}", english.shell_unsupported);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::ShellZdotdirUnsupported) => {
            eprintln!("{}", english.shell_zdotdir_unsupported);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::ShellVerificationFailed) => {
            eprintln!("{}", english.shell_verification_failed);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::CleanupUnknown) => {
            eprintln!("{}", english.cleanup_unknown);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::AdministratorUnavailable) => {
            eprintln!("{}", english.administrator_unavailable);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::NixPolicyRecoveryCompleted) => {
            eprintln!("{}", english.nix_policy_recovery_completed);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::NixPolicyRecoveryInspectionUnavailable) => {
            eprintln!("{}", english.nix_policy_recovery_inspection_unavailable);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::NixPolicyRecoveryPending) => {
            eprintln!("{}", english.nix_policy_recovery_pending);
            Exit { code: 1 }
        }
        Err(installation::TransactionError::CommandPathConflict { command, path }) => {
            eprintln!(
                "{}",
                english
                    .command_path_conflict
                    .replace("{command}", command.name())
                    .replace("{path}", &format!("{path:?}"))
            );
            Exit { code: 1 }
        }
        Err(installation::TransactionError::NixBlocked(reason)) => {
            use installation::nix_owner::NixBlock;
            let message = match reason {
                NixBlock::MissingClient => &english.nix_missing_client,
                NixBlock::UnprovedClient => &english.nix_unproved_client,
                NixBlock::UnprovedPlatform => &english.nix_unproved_platform,
                NixBlock::InactiveService => &english.nix_inactive_service,
                NixBlock::UnprovedService => &english.nix_unproved_service,
                NixBlock::DaemonUnavailable => &english.nix_daemon_unavailable,
                NixBlock::DaemonMismatch => &english.nix_daemon_mismatch,
                NixBlock::ConfigurationOwnerUnknown => &english.nix_configuration_owner_unknown,
                #[cfg(target_os = "linux")]
                NixBlock::CallerAccessDenied => &english.nix_caller_access_denied,
            };
            eprintln!("{message}");
            Exit { code: 1 }
        }
        Err(_) => {
            eprintln!("{}", english.operation_failed);
            Exit { code: 1 }
        }
    }
}

#[cfg(unix)]
fn write_review(
    output: &mut impl std::io::Write,
    summary: &installation::ReviewSummary,
) -> std::io::Result<()> {
    let copy = english();
    let operation = match summary.operation {
        installation::ReviewOperation::Install => &copy.review_install,
        installation::ReviewOperation::Update => &copy.review_update,
        installation::ReviewOperation::Rollback => &copy.review_rollback,
        installation::ReviewOperation::Uninstall => &copy.review_uninstall,
        installation::ReviewOperation::Recover => &copy.review_recover,
    };
    writeln!(output, "{operation}")?;
    if let Some(version) = &summary.version {
        writeln!(
            output,
            "{}",
            copy.review_version.replace("{version}", version)
        )?;
    }
    for change in &summary.changes {
        use installation::ReviewedChange;
        let message = match change {
            ReviewedChange::Uninstall => &copy.review_uninstall_changes,
            ReviewedChange::ShellPath => &copy.review_shell_path,
            ReviewedChange::ShellStartup => &copy.review_shell_startup,
            ReviewedChange::Recovery => &copy.review_recovery,
            ReviewedChange::CommandDirectory => &copy.review_command_directory,
            ReviewedChange::CommandLinks => &copy.review_command_links,
            ReviewedChange::Selection => &copy.review_selection,
            ReviewedChange::Retention => &copy.review_retention,
            ReviewedChange::Activation => &copy.review_activation,
            ReviewedChange::Cleanup => &copy.review_cleanup,
            ReviewedChange::NixPolicy => &copy.review_nix_policy,
            ReviewedChange::ExistingNixPolicy => &copy.review_existing_nix_policy,
            #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
            ReviewedChange::DarwinPolicyInspection => &copy.review_darwin_policy_inspection,
            #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
            ReviewedChange::DarwinPolicyPublication => &copy.review_darwin_policy_publication,
            #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
            ReviewedChange::DarwinPolicyRecoveryInspection => {
                &copy.review_darwin_policy_recovery_inspection
            }
            #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
            ReviewedChange::DarwinPolicyRestart => &copy.review_darwin_policy_restart,
            ReviewedChange::ApplyExistingNixPolicy => &copy.review_apply_existing_nix_policy,
            #[cfg(target_os = "linux")]
            ReviewedChange::NixPolicyRecoveryInspection => {
                &copy.review_nix_policy_recovery_inspection
            }
            #[cfg(target_os = "linux")]
            ReviewedChange::ResumePreparedNixPolicy => &copy.review_resume_prepared_nix_policy,
        };
        writeln!(output, "{message}")?;
    }
    if !matches!(summary.recovery, installation::RecoveryPending::None) {
        writeln!(output, "{}", copy.review_pending_recovery)?;
    }
    Ok(())
}

pub fn run() -> Exit {
    let arguments: Vec<_> = std::env::args_os().skip(1).collect();
    if let Some(exit) = dispatch_internal(&arguments) {
        return exit;
    }
    #[cfg(unix)]
    {
        use diagnostics::{
            DiagnosticOutcome, Invocation, InvocationResult, NativeRoute, RequestRoute,
        };
        let route = match arguments.as_slice() {
            [] => NativeRoute::NoArguments,
            [argument] if argument == "--help" || argument == "-h" => NativeRoute::Help,
            [argument] if argument == "--version" || argument == "-V" => NativeRoute::Version,
            _ => match arguments.first().and_then(|argument| argument.to_str()) {
                Some("install") => NativeRoute::Operation(RequestRoute::Install),
                Some("update") => NativeRoute::Operation(RequestRoute::Update),
                Some("rollback") => NativeRoute::Operation(RequestRoute::Rollback),
                Some("uninstall") => NativeRoute::Operation(RequestRoute::Uninstall),
                _ => NativeRoute::Unknown,
            },
        };
        diagnostics::record_public(Invocation::Native(route), || {
            let english = english();
            let (code, outcome) = match route {
                NativeRoute::NoArguments | NativeRoute::Help => {
                    println!("{}", english.help);
                    (0, DiagnosticOutcome::HelpDisplayed)
                }
                NativeRoute::Version => {
                    let identity = identity::BuildIdentity::current();
                    println!(
                        "{}",
                        english
                            .version
                            .replace("{version}", identity.product_version)
                            .replace("{revision}", identity.source_revision)
                            .replace("{target}", identity.target)
                    );
                    (0, DiagnosticOutcome::VersionDisplayed)
                }
                NativeRoute::Operation(_) => {
                    return run_request(&arguments, installation::Executor::NativeInstaller);
                }
                NativeRoute::Unknown => {
                    eprintln!("{}", english.invalid_arguments);
                    (2, DiagnosticOutcome::InvalidArguments)
                }
            };
            InvocationResult {
                exit: Exit { code },
                outcome,
            }
        })
    }
    #[cfg(not(unix))]
    run_non_unix(&arguments)
}

#[cfg(not(unix))]
fn run_non_unix(arguments: &[std::ffi::OsString]) -> Exit {
    let identity = identity::BuildIdentity::current();
    let english = english();

    let code = match arguments {
        [] => {
            println!("{}", english.help);
            0
        }
        [argument] if argument == "--help" || argument == "-h" => {
            println!("{}", english.help);
            0
        }
        [argument] if argument == "--version" || argument == "-V" => {
            println!(
                "{}",
                english
                    .version
                    .replace("{version}", identity.product_version)
                    .replace("{revision}", identity.source_revision)
                    .replace("{target}", identity.target)
            );
            0
        }
        _ => {
            eprintln!("{}", english.invalid_arguments);
            2
        }
    };

    Exit { code }
}

fn english() -> &'static language::English {
    ENGLISH.get_or_init(|| {
        serde_json::from_str(include_str!("english.json")).expect("valid English lifecycle text")
    })
}

#[cfg(all(test, unix))]
mod locator_tests {
    use super::*;

    #[test]
    fn bootstrap_parser_keeps_version_and_locator_distinct() {
        let locator = format!("0.17.2@{}", "a".repeat(64));
        let args = [
            "install",
            "--candidate-locator",
            &locator,
            "--version",
            "0.17.1",
            "--yes",
        ];
        let args = args.map(Into::into);
        let (request, options, executor) =
            parse_execution(&args, installation::Executor::NativeInstaller).unwrap();
        assert!(
            matches!(request, installation::Request::Install(Some(version)) if version == "0.17.1")
        );
        assert!(options.yes);
        assert!(matches!(executor, installation::Executor::Bootstrap(_)));
        assert!(parse_execution(&args, installation::Executor::InstalledNeo).is_err());
        for operation in ["update", "rollback", "uninstall"] {
            let args = [operation, "--candidate-locator", &locator].map(Into::into);
            assert!(parse_execution(&args, installation::Executor::NativeInstaller).is_err());
        }
        let args = [
            "install",
            "--candidate-locator",
            &locator,
            "--candidate-locator",
            &locator,
        ]
        .map(Into::into);
        assert!(parse_execution(&args, installation::Executor::NativeInstaller).is_err());
    }

    #[test]
    fn bootstrap_parser_rejects_ambiguous_and_unsafe_locators() {
        let sha = "a".repeat(64);
        for value in [
            format!("v1.2.3@{sha}"),
            format!("01.2.3@{sha}"),
            format!("1.2.3-rc.1@{sha}"),
            format!("1.2.3+build@{sha}"),
            format!("1.2.3@{}", "A".repeat(64)),
            format!("1.2.3@{sha}@{sha}"),
            format!("1.2.3@{sha}\n"),
            "1.2.3@../candidate.json".into(),
            "1.2.3@abc".into(),
        ] {
            let args = ["install", "--candidate-locator", &value].map(Into::into);
            assert!(
                parse_execution(&args, installation::Executor::NativeInstaller).is_err(),
                "accepted {value:?}"
            );
        }
    }
}
