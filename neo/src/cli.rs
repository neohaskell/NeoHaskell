use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "neo", version, about = "The NeoHaskell CLI")]
#[command(propagate_version = true)]
pub struct Cli {
    /// Enable debug-level output
    #[arg(short, long, global = true)]
    pub verbose: bool,

    /// Disable interactive prompts, animations, and colors
    #[arg(long, global = true)]
    pub ci: bool,

    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Scaffold a new NeoHaskell project
    #[command(long_about = "Scaffold a new NeoHaskell project with a full interactive interview. \
                            This command will guide you through project naming, versioning, \
                            and license selection. In --ci mode, it uses defaults unless args are provided.")]
    New {
        /// Project name (required in --ci mode)
        project_name: Option<String>,
        /// Scaffold a library project (no launcher folder, no executable cabal stanza)
        #[arg(long)]
        library: bool,
    },
    /// Reconcile config and build the project
    #[command(long_about = "Automatically generate Nix and Cabal files from neo.json and build the project. \
                            If --watch is used, it starts a GHCi session for instant feedback on file changes.")]
    Build {
        /// Watch mode with GHCi hot-reloading
        #[arg(long)]
        watch: bool,
        /// Skip the pre-build check that aborts when locked files have been modified
        #[arg(long)]
        skip_lock_check: bool,
    },
    /// Reconcile, build, and run the application
    #[command(long_about = "Build the project and execute the application. \
                            Use --watch to automatically rebuild and restart when source files change.")]
    Run {
        /// Watch mode with auto-restart
        #[arg(long)]
        watch: bool,
    },
    /// Run unit tests, then integration tests
    #[command(long_about = "Execute all unit tests via Cabal, followed by integration tests using Hurl. \
                            In --watch mode, tests are re-run on every file modification.")]
    Test {
        /// Watch mode for continuous testing
        #[arg(long)]
        watch: bool,
    },
    /// Lock event-sourced domain files
    #[command(long_about = "Search for and lock event-sourced domain files to prevent accidental modification. \
                            Locked files are added to .locked-files and verified by the pre-commit hook.")]
    Lock(LockArgs),
    /// Launch the bundled in-browser NeoHaskell IDE
    #[command(long_about = "Start a local HTTP server that serves the bundled NeoHaskell IDE \
                            (the Vite app embedded into the `neo` binary). Open the printed URL \
                            in your browser. Press Ctrl-C to stop. Defaults to binding 127.0.0.1 \
                            (loopback only). Pass `--host 0.0.0.0` to make the IDE reachable from \
                            other machines on your network.")]
    Ide {
        /// IP address to bind on (e.g. `127.0.0.1`, `0.0.0.0`, `::1`).
        /// Hostnames are not accepted — pass a literal IPv4 or IPv6 address.
        #[arg(long, default_value_t = std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST))]
        host: std::net::IpAddr,
        /// TCP port to bind
        #[arg(long, default_value_t = 2323)]
        port: u16,
    },
    /// Inspect a NeoHaskell project's domain layout (commands, events, queries, integrations)
    #[command(long_about = "Print a structured view of the project's NeoHaskell domains. \
                            By default emits everything as a single JSON document on stdout; \
                            use a subcommand to filter (e.g. `neo inspect commands` for just \
                            the command table). The heal flow for `event-model.json` uses the \
                            same data internally — running this command shows you exactly what \
                            the AI agent sees.")]
    Inspect {
        #[command(subcommand)]
        subcommand: Option<InspectSubcommand>,
    },
    /// Validate `event-model.json` against the schema + referential rules
    #[command(long_about = "Validate the project's `event-model.json` (the event model the `neo ide` \
                            authors) against the embedded JSON Schema and referential-integrity rules, \
                            without launching the browser IDE. Reads `<cwd>/event-model.json` by default, \
                            or the PATH you pass. Read-only — it never modifies the file.\n\n\
                            Exit codes: 0 = valid; 1 = could not read the file (IO error / permissions); \
                            2 = invalid (schema and/or referential errors); 3 = the file is not valid JSON; \
                            4 = the file does not exist. A missing model is a failure, not a no-op. \
                            Pass `--json` to emit the machine-readable validation result on stdout (the exit \
                            code still mirrors the status), e.g. `neo validate --json | jq -e '.status == \"valid\"'`.")]
    Validate {
        /// Path to the event model file (defaults to `<cwd>/event-model.json`)
        path: Option<PathBuf>,
        /// Emit the validation result as JSON on stdout instead of human-readable lines
        #[arg(long)]
        json: bool,
    },
    /// Install shared NeoHaskell skills into your AI coding tools
    #[command(long_about = "Fetch the shared skill library from github.com/neohaskell/skills and \
                            install the skills into the right project-root folders for the AI coding \
                            agents you use (Claude Code, OpenAI Codex, Kiro, Cursor). Run \
                            `neo skills setup` for an interactive tool picker; in --ci \
                            mode it installs for every supported tool unless you pass `--tool <id>`. \
                            If the library ships a primer (neohaskell.md), it is also installed next to \
                            each tool's skills and wired in: Claude imports it from CLAUDE.md via an \
                            `@`-import, Cursor gets a self-activating `.cursor/rules` rule, and Codex \
                            and Kiro inline it into a managed block in AGENTS.md; pass `--no-primer` \
                            to skip that.")]
    Skills {
        #[command(subcommand)]
        subcommand: Option<SkillsSubcommand>,
    },
}

/// Per-section views of the inspected project. `None` = dump the whole project.
#[derive(Subcommand)]
pub enum InspectSubcommand {
    /// List discovered domain directories under `src/`.
    Domains,
    /// List all commands per domain (name, file, events produced, HTTP-reachable flag).
    Commands,
    /// List event-sum constructors per domain.
    Events,
    /// List queries per domain (name, file, event constructors referenced).
    Queries,
    /// List integrations per domain (name, kind, events handled, commands emitted).
    Integrations,
    /// Derived wiring: which command produces which event, which integration listens to it, etc.
    Wiring,
    /// Force a code→model sync: refresh `event-model.json` from the project
    /// source (record fields, plus new nodes/edges). Editing fields of existing
    /// nodes is a data-only update with no layout change; a new node triggers a
    /// full re-layout. The `neo ide` background watcher runs the same sync.
    Sync,
}

/// `neo skills` actions. `None` runs `setup` with no flags.
#[derive(Subcommand)]
pub enum SkillsSubcommand {
    /// Fetch neohaskell/skills and install skills into your tools' folders
    Setup {
        /// Install for this tool (repeatable): claude, codex, kiro, cursor
        #[arg(long = "tool")]
        tools: Vec<String>,
        /// Install for every supported tool
        #[arg(long)]
        all_tools: bool,
        /// Install only this skill (repeatable); default installs every discovered skill
        #[arg(long = "skill")]
        skills: Vec<String>,
        /// Overwrite existing destinations without confirmation
        #[arg(long)]
        force: bool,
        /// Print the install plan without writing anything
        #[arg(long)]
        dry_run: bool,
        /// Re-clone the skills library instead of reusing the cached copy
        #[arg(long)]
        refresh: bool,
        /// Skip the always-on primer (neohaskell.md) and its instructions-file wiring
        #[arg(long)]
        no_primer: bool,
    },
}

#[derive(clap::Args)]
pub struct LockArgs {
    #[command(subcommand)]
    pub subcommand: Option<LockSubcommand>,

    /// Fuzzy search string to match domain files
    pub search: Option<String>,

    /// Lock all discovered domain files
    #[arg(long)]
    pub all: bool,
}

#[derive(Subcommand)]
pub enum LockSubcommand {
    /// Install the git pre-commit lock hook
    Install,
    /// Check if any locked files are being committed (used by pre-commit hook)
    Check,
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[test]
    fn test_parse_new() {
        let cli = Cli::try_parse_from(["neo", "new", "my-project"]).unwrap();
        match cli.command {
            Some(Commands::New { project_name, library }) => {
                assert_eq!(project_name, Some("my-project".into()));
                assert!(!library);
            }
            _ => panic!("Expected New command"),
        }
    }

    #[test]
    fn test_parse_new_library() {
        let cli = Cli::try_parse_from(["neo", "new", "my-lib", "--library"]).unwrap();
        match cli.command {
            Some(Commands::New { project_name, library }) => {
                assert_eq!(project_name, Some("my-lib".into()));
                assert!(library);
            }
            _ => panic!("Expected New command"),
        }
    }

    #[test]
    fn test_parse_ci_flag() {
        let cli = Cli::try_parse_from(["neo", "--ci", "build"]).unwrap();
        assert!(cli.ci);
        match cli.command {
            Some(Commands::Build { .. }) => (),
            _ => panic!("Expected Build command"),
        }
    }

    #[test]
    fn test_parse_build_skip_lock_check() {
        let cli = Cli::try_parse_from(["neo", "build", "--skip-lock-check"]).unwrap();
        match cli.command {
            Some(Commands::Build { watch, skip_lock_check }) => {
                assert!(!watch);
                assert!(skip_lock_check);
            }
            _ => panic!("Expected Build command"),
        }
    }

    #[test]
    fn test_parse_build_default_lock_check() {
        let cli = Cli::try_parse_from(["neo", "build"]).unwrap();
        match cli.command {
            Some(Commands::Build { watch, skip_lock_check }) => {
                assert!(!watch);
                assert!(!skip_lock_check);
            }
            _ => panic!("Expected Build command"),
        }
    }

    #[test]
    fn test_parse_run() {
        let cli = Cli::try_parse_from(["neo", "run", "--watch"]).unwrap();
        match cli.command {
            Some(Commands::Run { watch }) => assert!(watch),
            _ => panic!("Expected Run command"),
        }
    }

    #[test]
    fn test_parse_test() {
        let cli = Cli::try_parse_from(["neo", "test"]).unwrap();
        match cli.command {
            Some(Commands::Test { watch }) => assert!(!watch),
            _ => panic!("Expected Test command"),
        }
    }

    #[test]
    fn test_parse_inspect_sync() {
        let cli = Cli::try_parse_from(["neo", "inspect", "sync"]).unwrap();
        match cli.command {
            Some(Commands::Inspect { subcommand: Some(InspectSubcommand::Sync) }) => {}
            other => panic!("Expected Inspect Sync, got {:?}", other.is_some()),
        }
    }

    #[test]
    fn test_parse_validate() {
        let cli = Cli::try_parse_from(["neo", "validate"]).unwrap();
        match cli.command {
            Some(Commands::Validate { path, json }) => {
                assert!(path.is_none());
                assert!(!json);
            }
            _ => panic!("Expected Validate command"),
        }
    }

    #[test]
    fn test_parse_validate_with_path_and_json() {
        let cli = Cli::try_parse_from(["neo", "validate", "./model.json", "--json"]).unwrap();
        match cli.command {
            Some(Commands::Validate { path, json }) => {
                assert_eq!(path, Some(std::path::PathBuf::from("./model.json")));
                assert!(json);
            }
            _ => panic!("Expected Validate command"),
        }
    }

    #[test]
    fn test_parse_skills_setup() {
        let cli = Cli::try_parse_from([
            "neo", "skills", "setup",
            "--tool", "claude", "--tool", "cursor",
            "--skill", "foo",
            "--force", "--dry-run", "--refresh", "--no-primer",
        ])
        .unwrap();
        match cli.command {
            Some(Commands::Skills {
                subcommand: Some(SkillsSubcommand::Setup { tools, all_tools, skills, force, dry_run, refresh, no_primer }),
            }) => {
                assert_eq!(tools, vec!["claude".to_string(), "cursor".to_string()]);
                assert!(!all_tools);
                assert_eq!(skills, vec!["foo".to_string()]);
                assert!(force);
                assert!(dry_run);
                assert!(refresh);
                assert!(no_primer);
            }
            _ => panic!("Expected Skills Setup command"),
        }
    }

    #[test]
    fn test_parse_skills_setup_all_tools() {
        let cli = Cli::try_parse_from(["neo", "skills", "setup", "--all-tools"]).unwrap();
        match cli.command {
            Some(Commands::Skills {
                subcommand: Some(SkillsSubcommand::Setup { all_tools, tools, .. }),
            }) => {
                assert!(all_tools);
                assert!(tools.is_empty());
            }
            _ => panic!("Expected Skills Setup --all-tools"),
        }
    }

    #[test]
    fn test_parse_skills_bare() {
        let cli = Cli::try_parse_from(["neo", "skills"]).unwrap();
        match cli.command {
            Some(Commands::Skills { subcommand: None }) => {}
            _ => panic!("Expected bare Skills command"),
        }
    }

    #[test]
    fn test_parse_lock() {
        let cli = Cli::try_parse_from(["neo", "lock", "MyDomain"]).unwrap();
        match cli.command {
            Some(Commands::Lock(args)) => {
                assert_eq!(args.search, Some("MyDomain".to_string()));
            }
            _ => panic!("Expected Lock command"),
        }
    }

    #[test]
    fn test_parse_ide_defaults_to_loopback_and_2323() {
        let cli = Cli::try_parse_from(["neo", "ide"]).unwrap();
        match cli.command {
            Some(Commands::Ide { host, port }) => {
                assert_eq!(host, std::net::IpAddr::V4(std::net::Ipv4Addr::new(127, 0, 0, 1)));
                assert_eq!(port, 2323);
            }
            _ => panic!("Expected Ide command"),
        }
    }

    #[test]
    fn test_parse_ide_custom_port() {
        let cli = Cli::try_parse_from(["neo", "ide", "--port", "8080"]).unwrap();
        match cli.command {
            Some(Commands::Ide { host, port }) => {
                assert_eq!(host, std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST));
                assert_eq!(port, 8080);
            }
            _ => panic!("Expected Ide command"),
        }
    }

    #[test]
    fn test_parse_ide_custom_host_any_v4() {
        let cli = Cli::try_parse_from(["neo", "ide", "--host", "0.0.0.0"]).unwrap();
        match cli.command {
            Some(Commands::Ide { host, port }) => {
                assert_eq!(host, std::net::IpAddr::V4(std::net::Ipv4Addr::UNSPECIFIED));
                assert_eq!(port, 2323);
            }
            _ => panic!("Expected Ide command"),
        }
    }

    #[test]
    fn test_parse_ide_custom_host_v6() {
        let cli = Cli::try_parse_from(["neo", "ide", "--host", "::1", "--port", "9000"]).unwrap();
        match cli.command {
            Some(Commands::Ide { host, port }) => {
                assert_eq!(host, std::net::IpAddr::V6(std::net::Ipv6Addr::LOCALHOST));
                assert_eq!(port, 9000);
            }
            _ => panic!("Expected Ide command"),
        }
    }

    #[test]
    fn test_parse_ide_rejects_out_of_range_port() {
        // 99999 is outside u16 range; clap must refuse to parse.
        let result = Cli::try_parse_from(["neo", "ide", "--port", "99999"]);
        assert!(result.is_err(), "expected clap to reject port 99999");
    }

    #[test]
    fn test_parse_ide_rejects_hostname() {
        // `localhost` is a hostname, not an IP address. We require an IP literal so the
        // bind interface is unambiguous (no DNS, no v4-vs-v6 surprise).
        let result = Cli::try_parse_from(["neo", "ide", "--host", "localhost"]);
        assert!(result.is_err(), "expected clap to reject `localhost`");
    }

    #[test]
    fn test_parse_ide_rejects_garbage_host() {
        let result = Cli::try_parse_from(["neo", "ide", "--host", "not-an-ip"]);
        assert!(result.is_err(), "expected clap to reject `not-an-ip`");
    }
}
