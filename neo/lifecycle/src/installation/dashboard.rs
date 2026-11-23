mod input;
pub(super) mod terminal;
#[cfg(test)]
mod tests;

use super::coordinator::{Coordinator, OwnerInterest, ReviewDecision, ReviewOffer};
use super::observation::{Activity, Detail, PresentationObservation, Row};
use super::{Executor, Request, RunOutcome, TransactionError};
use input::{Input, Key};
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Paragraph, Widget};
use std::io::Write;
use std::os::fd::AsRawFd;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::task::Poll;
use std::time::{Duration, Instant};
use terminal::{Signals, Terminal};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DisplayPreference {
    Automatic,
    Still,
    Plain,
}
#[derive(Clone, Copy)]
pub(crate) struct Options {
    pub(crate) yes: bool,
    pub(crate) display: DisplayPreference,
}

pub(crate) struct DriveReport {
    pub(crate) operation: Result<RunOutcome, TransactionError>,
    pub(crate) cleanup: Vec<CleanupFailure>,
    pub(crate) presentation: Option<PresentationFailure>,
    pub(crate) observations: Option<crate::diagnostics::InvocationFacts>,
}
#[derive(Clone, Copy, Debug, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CleanupFailure {
    BackgroundWork,
    SignalHandlers,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Failure {
    Input,
    Output,
    Restore,
    Panic,
    Setup,
    DisplayData,
}
#[derive(Clone, Copy)]
pub(crate) struct PresentationFailure {
    pub(crate) primary: Failure,
    pub(crate) restored: bool,
}

struct SilentPanic(Option<Box<dyn Fn(&std::panic::PanicHookInfo<'_>) + Send + Sync>>);
impl SilentPanic {
    fn new() -> Self {
        let previous = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        Self(Some(previous))
    }
}
impl Drop for SilentPanic {
    fn drop(&mut self) {
        if let Some(previous) = self.0.take() {
            std::panic::set_hook(previous);
        }
    }
}

pub(crate) fn run(request: Request, executor: Executor, options: Options) -> DriveReport {
    let terminal = if options.yes || options.display == DisplayPreference::Plain {
        None
    } else {
        Terminal::probe().ok().flatten()
    };
    let mut signals = match Signals::new() {
        Ok(signals) => signals,
        Err(_) => {
            return DriveReport {
                cleanup: Vec::new(),
                observations: None,
                operation: Err(TransactionError::Io),
                presentation: Some(PresentationFailure {
                    primary: Failure::Setup,
                    restored: true,
                }),
            };
        }
    };
    let _panic = SilentPanic::new();
    #[cfg(test)]
    let mut owner = if tests::fault_mode("signal-restore-primary") {
        tests::run_owner_fixture(request, executor)
    } else {
        Coordinator::new(request, executor, None)
    };
    #[cfg(not(test))]
    let mut owner = Coordinator::new(request, executor, None);
    let mut report = protected(&mut owner, terminal, &signals, options);
    if catch_unwind(AssertUnwindSafe(|| drop(owner))).is_err() {
        report.cleanup.push(CleanupFailure::BackgroundWork);
    }
    if !signals.restore() {
        report.cleanup.push(CleanupFailure::SignalHandlers);
    }
    report
}

fn protected(
    owner: &mut Coordinator,
    mut terminal: Option<Terminal>,
    signals: &Signals,
    options: Options,
) -> DriveReport {
    if let Some(tty) = &mut terminal {
        if tty.activate().is_err() {
            if !tty.restore() {
                return DriveReport {
                    cleanup: Vec::new(),
                    observations: Some(owner.diagnostics()),
                    operation: Err(TransactionError::Io),
                    presentation: Some(PresentationFailure {
                        primary: Failure::Setup,
                        restored: false,
                    }),
                };
            }
            terminal = None;
        }
    }
    let attempt = catch_unwind(AssertUnwindSafe(|| {
        drive(owner, &mut terminal, signals, options)
    }));
    let failure = match attempt {
        Ok(Ok(operation)) => {
            let restored = terminal.as_mut().is_none_or(Terminal::restore);
            return DriveReport {
                cleanup: Vec::new(),
                observations: Some(owner.diagnostics()),
                operation,
                presentation: (!restored).then_some(PresentationFailure {
                    primary: Failure::Restore,
                    restored,
                }),
            };
        }
        Ok(Err(failure)) => failure,
        Err(_) => Failure::Panic,
    };
    let restored = terminal.as_mut().is_none_or(Terminal::restore);
    owner.request_cancel();
    let operation = catch_unwind(AssertUnwindSafe(|| settle(owner, signals)))
        .unwrap_or(Err(TransactionError::CleanupUnknown));
    DriveReport {
        cleanup: Vec::new(),
        observations: Some(owner.diagnostics()),
        operation,
        presentation: Some(PresentationFailure {
            primary: failure,
            restored,
        }),
    }
}

fn settle(owner: &mut Coordinator, signals: &Signals) -> Result<RunOutcome, TransactionError> {
    #[cfg(test)]
    if tests::fault_mode("cleanup-unwind") {
        panic!("PRIVATE-DASHBOARD-CLEANUP-PANIC");
    }
    loop {
        signals.drain();
        if let Poll::Ready(result) = owner.advance() {
            return result;
        }
        wait_owner(owner, signals, None, None)?;
    }
}

struct Frame {
    bytes: Vec<u8>,
    offset: usize,
    deadline: Instant,
    review: Option<PresentationStamp>,
    #[cfg(test)]
    test_page: Page,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct PresentationStamp {
    input: u64,
    page: Page,
    viewport: (u16, u16),
    revision: u64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Page {
    Summary,
    Details,
}

fn drive(
    owner: &mut Coordinator,
    terminal: &mut Option<Terminal>,
    signals: &Signals,
    options: Options,
) -> Result<Result<RunOutcome, TransactionError>, Failure> {
    let mut input = Input::new();
    let mut offer: Option<ReviewOffer> = None;
    let mut page = Page::Summary;
    let mut scroll = 0usize;
    let mut review_page = 0usize;
    let mut armed = None;
    let mut frame: Option<Frame> = None;
    let mut dirty = true;
    let mut animation = Instant::now();
    let mut tick = 0usize;
    let mut runnable = true;
    let mut viewport = None;
    let mut revision = 0u64;
    loop {
        let (cancel, resized) = signals.drain();
        if cancel {
            owner.request_cancel();
            armed = None;
            dirty = true;
        }
        if let Some(tty) = terminal.as_ref() {
            if !tty.foreground() {
                return Err(Failure::Input);
            }
            let measured = tty.size().map_err(|_| Failure::Input)?;
            if resized || viewport != Some(measured) {
                review_page = 0;
                viewport = Some(measured);
                revision = revision.wrapping_add(1);
                armed = None;
                input.reset();
                tty.discard_input().map_err(|_| Failure::Input)?;
                dirty = true;
            }
        }
        let now = Instant::now();
        let keys = if let Some(tty) = terminal.as_ref() {
            let bytes = tty.read().map_err(|_| Failure::Input)?;
            #[cfg(test)]
            tests::inject(&bytes, owner);
            input.decode(&bytes, now)
        } else {
            Vec::new()
        };
        if keys.contains(&Key::Cancel) {
            owner.request_cancel();
            armed = None;
            dirty = true;
        } else if let Some(key) = keys.first() {
            match key {
                Key::Details | Key::Back | Key::Accept if page == Page::Details => {
                    page = Page::Summary;
                    armed = None;
                    input.reset();
                    dirty = true;
                }
                Key::Details
                    if offer.is_none()
                        || !offer.as_ref().is_some_and(|r| install_review(r.summary())) =>
                {
                    page = Page::Details;
                    armed = None;
                    input.reset();
                    dirty = true;
                }
                Key::Up if page == Page::Details => {
                    scroll = scroll.saturating_add(1);
                    dirty = true;
                }
                Key::Down if page == Page::Details => {
                    scroll = scroll.saturating_sub(1);
                    dirty = true;
                }
                Key::Back | Key::Accept | Key::Install
                    if offer.is_some()
                        && armed
                            == viewport.map(|viewport| PresentationStamp {
                                input: input.generation(),
                                page,
                                viewport,
                                revision,
                            }) =>
                {
                    let (cancel, resized) = signals.drain();
                    let measured = terminal
                        .as_ref()
                        .ok_or(Failure::Setup)?
                        .size()
                        .map_err(|_| Failure::Input)?;
                    if cancel {
                        owner.request_cancel();
                    } else if resized || viewport != Some(measured) {
                        review_page = 0;
                        viewport = Some(measured);
                        revision = revision.wrapping_add(1);
                    } else {
                        let summary = offer.as_ref().ok_or(Failure::Setup)?.summary();
                        let decision = if install_review(summary) {
                            let pages = review_pages(measured, summary)?;
                            let last = review_page + 1 == pages.len();
                            match key {
                                Key::Accept if !last => {
                                    review_page += 1;
                                    revision = revision.wrapping_add(1);
                                    None
                                }
                                Key::Install if last => Some(ReviewDecision::Accept),
                                Key::Back if review_page > 0 => {
                                    review_page -= 1;
                                    revision = revision.wrapping_add(1);
                                    None
                                }
                                Key::Back => Some(ReviewDecision::Decline),
                                _ => None,
                            }
                        } else {
                            match key {
                                Key::Accept => Some(ReviewDecision::Accept),
                                Key::Back => Some(ReviewDecision::Decline),
                                _ => None,
                            }
                        };
                        if let Some(decision) = decision {
                            let review = offer.take().ok_or(Failure::Setup)?;
                            if matches!(decision, ReviewDecision::Accept)
                                && cache_change(review.summary()).is_some()
                            {
                                let tty = terminal.as_mut().ok_or(Failure::Setup)?;
                                if !tty.restore() {
                                    return Err(Failure::Input);
                                }
                                let authorized = std::process::Command::new("/usr/bin/sudo")
                                    .arg("-v")
                                    .env_clear()
                                    .env("PATH", "/usr/bin:/bin")
                                    .status()
                                    .is_ok_and(|status| status.success());
                                tty.activate().map_err(|_| Failure::Setup)?;
                                if !authorized {
                                    return Ok(Err(TransactionError::NixBlocked(
                                        super::nix::NixBlock::CacheConfiguration,
                                    )));
                                }
                            }
                            match owner.answer(review, decision) {
                                Ok(()) | Err(TransactionError::StopRequested) => {}
                                Err(_) => return Err(Failure::DisplayData),
                            }
                        }
                    }
                    armed = None;
                    input.reset();
                    dirty = true;
                }
                _ => {}
            }
        }
        if runnable || matches!(owner.interest(), OwnerInterest::Ready) {
            if let Poll::Ready(result) = owner.advance() {
                return Ok(result);
            }
            dirty = true;
        }
        if offer.is_none() {
            if let Some(review) = owner.take_review() {
                review_page = 0;
                offer = Some(review);
                page = Page::Summary;
                armed = None;
                input.reset();
                dirty = true;
                if let Some(tty) = terminal.as_ref() {
                    tty.discard_input().map_err(|_| Failure::Input)?;
                }
            }
        }
        if terminal.is_none() {
            if let Some(review) = offer.take() {
                crate::write_review(&mut std::io::stdout().lock(), review.summary())
                    .and_then(|_| std::io::stdout().flush())
                    .map_err(|_| Failure::Output)?;
                owner
                    .answer(
                        review,
                        if options.yes {
                            ReviewDecision::Accept
                        } else {
                            ReviewDecision::ConsentRequired
                        },
                    )
                    .map_err(|_| Failure::DisplayData)?;
            }
        } else {
            let tty = terminal.as_ref().ok_or(Failure::Setup)?;
            let animate = options.display != DisplayPreference::Still && offer.is_none();
            if animate && now >= animation {
                tick = tick.wrapping_add(1);
                animation = now + Duration::from_millis(250);
                dirty = true;
            }
            if dirty && frame.is_none() {
                let size = viewport.ok_or(Failure::Setup)?;
                let rendered = render(
                    size,
                    &owner.presentation(),
                    offer.as_ref(),
                    page,
                    if page == Page::Summary {
                        review_page
                    } else {
                        scroll
                    },
                    tick,
                )?;
                if let Some(bytes) = rendered {
                    frame = Some(Frame {
                        bytes,
                        #[cfg(test)]
                        test_page: page,
                        offset: 0,
                        deadline: now + Duration::from_secs(2),
                        review: (offer.is_some() && page == Page::Summary && armed.is_none())
                            .then_some(PresentationStamp {
                                input: input.generation(),
                                page,
                                viewport: size,
                                revision,
                            }),
                    });
                    dirty = false;
                } else {
                    #[cfg(test)]
                    tests::prepare_small_layout_fallback();
                    if !terminal.as_mut().ok_or(Failure::Setup)?.restore() {
                        return Err(Failure::Restore);
                    }
                    *terminal = None;
                    frame = None;
                    armed = None;
                    input.reset();
                    continue;
                }
            }
            if let Some(pending) = &mut frame {
                if now >= pending.deadline {
                    return Err(Failure::Output);
                }
                #[cfg(test)]
                let limit = tests::write_limit(
                    pending.bytes.len(),
                    pending.test_page,
                    pending.review.is_some(),
                );
                #[cfg(not(test))]
                let limit = pending.bytes.len();
                match tty.write(
                    &pending.bytes[pending.offset..pending.bytes.len().min(pending.offset + limit)],
                ) {
                    Ok(0) => return Err(Failure::Output),
                    Ok(count) => pending.offset += count,
                    Err(error)
                        if matches!(
                            error.kind(),
                            std::io::ErrorKind::WouldBlock | std::io::ErrorKind::Interrupted
                        ) => {}
                    Err(_) => return Err(Failure::Output),
                }
                #[cfg(test)]
                tests::partial_checkpoint(
                    pending.test_page,
                    pending.review.is_some(),
                    pending.offset,
                    pending.bytes.len(),
                );
                if pending.offset == pending.bytes.len() {
                    if pending.review
                        == viewport.map(|viewport| PresentationStamp {
                            input: input.generation(),
                            page,
                            viewport,
                            revision,
                        })
                        && page == Page::Summary
                        && offer.is_some()
                        && viewport == Some(tty.size().map_err(|_| Failure::Input)?)
                    {
                        tty.discard_input().map_err(|_| Failure::Input)?;
                        input.reset();
                        armed = viewport.map(|viewport| PresentationStamp {
                            input: input.generation(),
                            page,
                            viewport,
                            revision,
                        });
                    }
                    #[cfg(test)]
                    tests::frame_checkpoint(armed.is_some());
                    frame = None;
                }
            }
        }
        if terminal.is_some() && dirty && frame.is_none() {
            runnable = false;
            continue;
        }
        let redraw =
            (terminal.is_some() && options.display != DisplayPreference::Still && offer.is_none())
                .then_some(animation);
        let deadline = [
            input.deadline(),
            frame.as_ref().map(|pending| pending.deadline),
            redraw,
        ]
        .into_iter()
        .flatten()
        .min();
        let tty_interest = terminal.as_ref().map(|tty| {
            (
                tty.descriptor(),
                libc::POLLIN | if frame.is_some() { libc::POLLOUT } else { 0 },
            )
        });
        runnable =
            wait_owner(owner, signals, tty_interest, deadline).map_err(|_| Failure::Input)?;
    }
}

fn wait_owner(
    owner: &Coordinator,
    signals: &Signals,
    tty: Option<(i32, i16)>,
    deadline: Option<Instant>,
) -> Result<bool, TransactionError> {
    let interest = owner.interest();
    let OwnerInterest::Waiting {
        descriptor,
        deadline: owner_deadline,
    } = interest
    else {
        return Ok(true);
    };
    let mut descriptors = signals
        .descriptors()
        .map(|fd| libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        })
        .to_vec();
    if let Some((fd, events)) = tty {
        descriptors.push(libc::pollfd {
            fd,
            events,
            revents: 0,
        });
    }
    let owner_index = descriptors.len();
    if let Some((fd, events)) = descriptor {
        descriptors.push(libc::pollfd {
            fd: fd.as_raw_fd(),
            events,
            revents: 0,
        });
    }
    terminal::wait(
        &mut descriptors,
        [deadline, owner_deadline].into_iter().flatten().min(),
    )
    .map_err(|_| TransactionError::Io)?;
    if tty.is_some() && descriptors[2].revents & (libc::POLLERR | libc::POLLHUP) != 0 {
        return Err(TransactionError::Io);
    }
    Ok(descriptors
        .get(owner_index)
        .is_some_and(|fd| fd.revents != 0)
        || owner_deadline.is_some_and(|deadline| Instant::now() >= deadline))
}

const SURFACE: Color = Color::Rgb(0x1c, 0x18, 0x26);
const PRIMARY: Color = Color::Rgb(0xf5, 0xef, 0xff);
const MUTED: Color = Color::Rgb(0xb5, 0xa9, 0xc9);
const LILAC: Color = Color::Rgb(0xc6, 0xa0, 0xff);
const MINT: Color = Color::Rgb(0x9e, 0xe6, 0xbc);
const PINK: Color = Color::Rgb(0xf3, 0x9b, 0xc4);
const BADGE_TEXT: Color = Color::Rgb(0x21, 0x14, 0x2c);

fn activity_text(activity: Activity) -> &'static str {
    &crate::english().dashboard.activities[activity as usize]
}

fn wrap(text: &str, width: usize) -> Vec<String> {
    let mut result = Vec::new();
    for source in text.lines() {
        let mut line = String::new();
        for word in source.split_whitespace() {
            if !line.is_empty() && line.len() + 1 + word.len() > width {
                result.push(std::mem::take(&mut line));
            }
            if !line.is_empty() {
                line.push(' ');
            }
            for chunk in word.as_bytes().chunks(width) {
                if line.len() + chunk.len() > width {
                    result.push(std::mem::take(&mut line));
                }
                line.push_str(std::str::from_utf8(chunk).unwrap_or(""));
            }
        }
        result.push(line);
    }
    result
}

fn cache_change(summary: &super::ReviewSummary) -> Option<(&str, &str)> {
    summary.changes.iter().find_map(|change| match change {
        super::ReviewedChange::CacheSetup { target, additions } => {
            Some((*target, additions.as_str()))
        }
        _ => None,
    })
}

fn install_review(summary: &super::ReviewSummary) -> bool {
    summary.operation == super::ReviewOperation::Install || cache_change(summary).is_some()
}

struct ReviewPage {
    text: String,
    controls: &'static str,
}

fn review_pages(
    size: (u16, u16),
    summary: &super::ReviewSummary,
) -> Result<Vec<ReviewPage>, Failure> {
    let width = usize::from(size.0.min(76).saturating_sub(8)).max(1);
    let rows = usize::from(size.1)
        .saturating_sub(9 + wrap("[I] Authorize and install    [Esc] Back", width).len())
        .max(1);
    let mut pages = Vec::new();
    let mut section = |text: &str, next: &'static str| {
        let lines = wrap(text, width);
        let mut chunks = lines.chunks(rows).peekable();
        while let Some(lines) = chunks.next() {
            pages.push(ReviewPage {
                text: lines.join("\n"),
                controls: if chunks.peek().is_some() {
                    "[Enter] Read more    [Q] Cancel"
                } else {
                    next
                },
            });
        }
    };
    let active = summary
        .changes
        .contains(&super::ReviewedChange::AlreadyActive);
    let action = if active {
        "Check"
    } else if summary.operation == super::ReviewOperation::Update {
        "Update to"
    } else {
        "Install"
    };
    let version = summary.version.as_deref().unwrap_or("");
    let mut text = format!("{action} NeoHaskell {version}?\n");
    if active {
        text.push_str("This version is already active.\n");
    }
    text.push_str("\nAfter you approve, the installer will:");
    let cache = cache_change(summary);
    let mut steps = Vec::new();
    if let Some((target, additions)) = cache {
        text.push_str("\n\nFor everyone using Nix on this machine");
        let mut settings = Vec::new();
        for line in additions.lines() {
            if let Some((name, values)) = line.split_once(" = ") {
                match name {
                    "extra-substituters" => settings.push("package sources".to_owned()),
                    "extra-trusted-public-keys" => settings.push("signing keys".to_owned()),
                    "extra-experimental-features" => settings.push(format!("features ({values})")),
                    _ => {}
                }
            }
        }
        steps.push(format!(
            "Add the missing Nix settings to {target} using sudo: {}.",
            settings.join(", ")
        ));
        steps.push("Restart Nix to load the settings.".to_owned());
    }
    let system_steps = steps.len();
    if summary.changes.contains(&super::ReviewedChange::Recovery)
        || !matches!(summary.recovery, super::RecoveryPending::None)
    {
        steps.push("Finish the interrupted installation changes.".to_owned());
    }
    if !active {
        steps.push("Get neo and neoctl and verify both packages.".to_owned());
    }
    steps.push("Set up both commands in ~/.local/bin and update PATH if needed.".to_owned());
    steps.push("Open fresh shells and check both commands.".to_owned());
    for (index, step) in steps.iter().enumerate() {
        if index == system_steps {
            text.push_str("\n\nFor your account");
        }
        text.push_str(&format!("\n{}. {step}", index + 1));
    }
    if let Some((target, additions)) = cache {
        section(&text, "[Enter] Review Nix settings    [Q] Cancel");
        section(
            &format!(
                "Nix settings for this machine\n\nThe installer will add these lines to {target}:\n{additions}\nIt will then restart Nix. You may be asked for your sudo password."
            ),
            "[I] Authorize and install    [Esc] Back",
        );
    } else {
        section(
            &text,
            if active {
                "[I] Check setup    [Esc] Cancel"
            } else {
                "[I] Install    [Esc] Cancel"
            },
        );
    }
    Ok(pages)
}

fn render(
    size: (u16, u16),
    observation: &PresentationObservation,
    offer: Option<&ReviewOffer>,
    page: Page,
    scroll: usize,
    tick: usize,
) -> Result<Option<Vec<u8>>, Failure> {
    let copy = &crate::english().dashboard;
    let width = size.0.min(76);
    if width <= 12 {
        return Ok(None);
    }
    let inner = usize::from(width - 8);
    let mut lines: Vec<Line<'static>> = vec![Line::from(vec![
        Span::styled(
            format!(" {} ", copy.short_name),
            Style::default()
                .fg(BADGE_TEXT)
                .bg(PINK)
                .add_modifier(Modifier::BOLD),
        ),
        Span::styled(
            format!("  {}", copy.brand),
            Style::default().fg(PRIMARY).add_modifier(Modifier::BOLD),
        ),
    ])];
    let mut add = |text: &str, color: Color| {
        lines.extend(
            wrap(text, inner)
                .into_iter()
                .map(|text| Line::from(Span::styled(text, Style::default().fg(color)))),
        );
    };

    add("", PRIMARY);
    if page == Page::Details {
        add(&copy.details, PRIMARY);
        let entries: Vec<_> = observation.details().rev().skip(scroll).take(8).collect();
        for detail in entries.into_iter().rev() {
            add(
                match detail {
                    Detail::Activity(activity) => activity_text(*activity),
                    Detail::Truncated => &copy.truncated,
                },
                MUTED,
            );
        }
        add("", PRIMARY);
        add(&copy.details_controls, LILAC);
    } else if let Some(offer) = offer {
        if install_review(offer.summary()) {
            let pages = review_pages(size, offer.summary())?;
            let current = &pages[scroll.min(pages.len() - 1)];
            add(&current.text, PRIMARY);
            add("", PRIMARY);
            add(current.controls, LILAC);
        } else {
            let mut text = Vec::new();
            crate::write_review(&mut text, offer.summary()).map_err(|_| Failure::DisplayData)?;
            let text = String::from_utf8(text).map_err(|_| Failure::DisplayData)?;
            add(&text, PRIMARY);
            add("", PRIMARY);
            add(&copy.review_controls, LILAC);
        }
    } else if matches!(
        observation.activity,
        Activity::Inspecting
            | Activity::CheckingNix
            | Activity::Selecting
            | Activity::PreparingReview
    ) {
        add("Checking your setup...", PRIMARY);
        if observation.rows[0] == Row::Existing {
            add("Nix is running.", MINT);
        }
        add(
            "You'll review the changes before installation starts.",
            MUTED,
        );
        add("[D] Details    [Q] Cancel", LILAC);
    } else {
        for (index, row) in observation.rows.iter().enumerate() {
            if *row == Row::NotObserved {
                continue;
            }
            let status = &copy.statuses[*row as usize];
            let color = match row {
                Row::Verified | Row::Existing => MINT,
                Row::Blocked => PINK,
                Row::Working => LILAC,
                _ => MUTED,
            };
            add(&format!("{status:<9} {}", copy.rows[index]), color);
        }
        add("", PRIMARY);
        add(activity_text(observation.activity), PRIMARY);
        if observation.rows.contains(&Row::Working) {
            add(if tick % 2 == 0 { "|" } else { "/" }, LILAC);
        }
        add("", PRIMARY);
        add(&copy.controls, LILAC);
    }
    let height = (lines.len() as u16 + 4).max(10);
    if height > size.1 {
        return Ok(None);
    }
    let viewport = Rect::new(0, 0, size.0, size.1);
    let panel = if offer.is_some() {
        Rect::new(
            (size.0 - width) / 2,
            (size.1 - height).min(1),
            width,
            height,
        )
    } else {
        Rect::new(0, 0, width, height)
    };
    let mut buffer = Buffer::empty(viewport);
    buffer.set_style(viewport, Style::default().bg(SURFACE).fg(PRIMARY));
    Block::default()
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(LILAC))
        .title(" NeoHaskell installer ")
        .render(panel, &mut buffer);
    Paragraph::new(lines).render(
        Rect::new(panel.x + 4, panel.y + 2, width - 8, height - 4),
        &mut buffer,
    );
    encode(&buffer).map(Some)
}

struct BoundedFrame(Vec<u8>);
impl Write for BoundedFrame {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        if self.0.len().saturating_add(bytes.len()) > 256 * 1024 {
            return Err(std::io::ErrorKind::OutOfMemory.into());
        }
        self.0.extend_from_slice(bytes);
        Ok(bytes.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn encode(buffer: &Buffer) -> Result<Vec<u8>, Failure> {
    let mut bytes = BoundedFrame(Vec::new());
    let mut style = None;
    for y in buffer.area.y..buffer.area.bottom() {
        write!(bytes, "\x1b[{};1H", y + 1).map_err(|_| Failure::Output)?;
        for x in buffer.area.x..buffer.area.right() {
            let cell = &buffer[(x, y)];
            if style != Some((cell.fg, cell.bg, cell.modifier)) {
                bytes
                    .write_all(if cell.modifier.contains(Modifier::BOLD) {
                        b"\x1b[1m"
                    } else {
                        b"\x1b[22m"
                    })
                    .map_err(|_| Failure::Output)?;
                for (color, prefix) in [(cell.fg, 38), (cell.bg, 48)] {
                    if let Color::Rgb(r, g, b) = color {
                        write!(bytes, "\x1b[{prefix};2;{r};{g};{b}m")
                            .map_err(|_| Failure::Output)?;
                    }
                }
                style = Some((cell.fg, cell.bg, cell.modifier));
            }
            bytes
                .write_all(cell.symbol().as_bytes())
                .map_err(|_| Failure::Output)?;
        }
    }
    bytes.write_all(b"\x1b[0m").map_err(|_| Failure::Output)?;
    Ok(bytes.0)
}

#[cfg(test)]
static ADVANCES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
#[cfg(test)]
pub(super) fn note_advance() {
    ADVANCES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}
