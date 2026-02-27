use clap::Parser;
use std::process;

fn main() {
    let cli = neo_install::Cli::parse();
    neo_install::ui::print_header();

    if let Err(e) = neo_install::run(&cli) {
        eprintln!("{} {e}", console::style("error:").red().bold());
        process::exit(1);
    }
}
