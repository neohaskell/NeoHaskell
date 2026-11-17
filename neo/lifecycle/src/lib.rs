mod candidate;
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

pub fn installed_help() -> &'static str {
    &english().installed_lifecycle_help
}

pub fn dispatch_installed(arguments: &[std::ffi::OsString]) -> Option<Exit> {
    if let Some(exit) = dispatch_internal(arguments) {
        return Some(exit);
    }
    #[cfg(unix)]
    if arguments
        .first()
        .is_some_and(|argument| argument == "update" || argument == "rollback")
    {
        if arguments.len() == 2 && (arguments[1] == "--help" || arguments[1] == "-h") {
            println!("{}", installed_help());
            return Some(Exit { code: 0 });
        }
        return Some(run_request(arguments, installation::Executor::InstalledNeo));
    }
    None
}

#[cfg(unix)]
fn parse_invocation(arguments: &[std::ffi::OsString]) -> Result<(installation::Request, bool), ()> {
    let mut consent = false;
    let mut filtered = Vec::new();
    for argument in arguments {
        if argument == "--yes" {
            if consent {
                return Err(());
            }
            consent = true;
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
    Ok((request, consent))
}

#[cfg(unix)]
fn run_request(arguments: &[std::ffi::OsString], executor: installation::Executor) -> Exit {
    let english = english();
    if arguments.len() == 2 && (arguments[1] == "--help" || arguments[1] == "-h") {
        println!("{}", english.help);
        return Exit { code: 0 };
    }
    let (request, consent) = match parse_invocation(arguments) {
        Ok(invocation) => invocation,
        Err(()) => {
            eprintln!("{}", english.invalid_request);
            return Exit { code: 2 };
        }
    };
    format_result(installation::run(request, executor, consent))
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
            }
            Exit { code: 130 }
        }
        Err(installation::TransactionError::CleanupUnknown) => {
            eprintln!("{}", english.cleanup_unknown);
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
            ReviewedChange::Recovery => &copy.review_recovery,
            ReviewedChange::CommandDirectory => &copy.review_command_directory,
            ReviewedChange::CommandLinks => &copy.review_command_links,
            ReviewedChange::Selection => &copy.review_selection,
            ReviewedChange::Retention => &copy.review_retention,
            ReviewedChange::Activation => &copy.review_activation,
            ReviewedChange::Cleanup => &copy.review_cleanup,
        };
        writeln!(output, "{message}")?;
    }
    if !matches!(summary.recovery, installation::RecoveryPending::None) {
        writeln!(output, "{}", copy.review_pending_recovery)?;
    }
    Ok(())
}

pub fn run() -> Exit {
    let identity = identity::BuildIdentity::current();
    let arguments: Vec<_> = std::env::args_os().skip(1).collect();
    if let Some(exit) = dispatch_internal(&arguments) {
        return exit;
    }
    let english = english();

    let code = match arguments.as_slice() {
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
        #[cfg(unix)]
        _ if arguments.first().is_some_and(|argument| {
            argument == "install" || argument == "update" || argument == "rollback"
        }) =>
        {
            return run_request(&arguments, installation::Executor::NativeInstaller);
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
