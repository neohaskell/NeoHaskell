use console::style;
use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};
use std::time::Duration;

pub const SOURCE_HEADER: &str = "🔍 Source: https://github.com/neohaskell/NeoHaskell";
pub const ISSUES_URL: &str = "https://github.com/neohaskell/NeoHaskell/issues";
pub const MSG_INSTALLING: &str = "🚀 Installing NeoHaskell...";
pub const MSG_TOOLCHAIN: &str = "⚙️  Setting up toolchain...";
pub const MSG_NEO_CLI: &str = "📦 Installing Neo CLI...";
pub const MSG_VERIFYING: &str = "🔍 Verifying installation...";
pub const MSG_DONE: &str = "✅ Done! Run 'neo new myproject' to get started";

pub fn create_spinner(msg: &str) -> ProgressBar {
    let pb = ProgressBar::new_spinner();
    if !is_tty() {
        pb.set_draw_target(ProgressDrawTarget::hidden());
    }
    pb.set_style(
        ProgressStyle::default_spinner()
            .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"])
            .template("{spinner:.blue} {msg}")
            .expect("valid spinner template"),
    );
    pb.set_message(msg.to_string());
    pb.enable_steady_tick(Duration::from_millis(120));
    pb
}

pub fn finish_success(pb: &ProgressBar, msg: &str) {
    pb.finish_with_message(format!("{} {msg}", style("✔").green()));
}

pub fn finish_error(pb: &ProgressBar, msg: &str) {
    pb.finish_with_message(format!("{} {msg}", style("✘").red()));
}

pub fn print_header() {
    println!("{SOURCE_HEADER}");
}

pub fn print_step(msg: &str, verbose: bool) {
    if verbose {
        println!("{}", style(msg).bold());
    }
}

fn is_tty() -> bool {
    std::io::IsTerminal::is_terminal(&std::io::stderr())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_header_contains_repo_url() {
        assert!(SOURCE_HEADER.contains("https://github.com/neohaskell/NeoHaskell"));
    }

    #[test]
    fn issues_url_is_correct() {
        assert_eq!(
            ISSUES_URL,
            "https://github.com/neohaskell/NeoHaskell/issues"
        );
    }

    #[test]
    fn msg_constants_are_non_empty() {
        assert!(!MSG_INSTALLING.is_empty());
        assert!(!MSG_TOOLCHAIN.is_empty());
        assert!(!MSG_NEO_CLI.is_empty());
        assert!(!MSG_VERIFYING.is_empty());
        assert!(!MSG_DONE.is_empty());
    }

    #[test]
    fn msg_done_mentions_neo_new() {
        assert!(MSG_DONE.contains("neo new"));
    }

    #[test]
    fn create_spinner_returns_progress_bar() {
        let pb = create_spinner("test");
        pb.finish_and_clear();
    }

    #[test]
    fn finish_success_does_not_panic() {
        let pb = ProgressBar::new_spinner();
        pb.set_draw_target(ProgressDrawTarget::hidden());
        finish_success(&pb, "ok");
    }

    #[test]
    fn finish_error_does_not_panic() {
        let pb = ProgressBar::new_spinner();
        pb.set_draw_target(ProgressDrawTarget::hidden());
        finish_error(&pb, "fail");
    }
}
