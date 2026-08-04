use crate::cli::Commands;
use crate::commands;
use crate::output::OutputMode;
use ratatui::{crossterm::event::{self, Event}, Frame, DefaultTerminal};
use std::time::Duration;
use miette::IntoDiagnostic;

pub enum Action<T> {
    Continue,
    Quit(T),
}

pub trait State {
    type Output;
    fn view(&self, frame: &mut Frame);
    fn update(&mut self, event: Event) -> miette::Result<Action<Self::Output>>;
    fn tick(&mut self);
}

pub struct App<'a, S: State> {
    pub state: S,
    pub terminal: &'a mut DefaultTerminal,
}

impl<'a, S: State> App<'a, S> {
    pub fn new(state: S, terminal: &'a mut DefaultTerminal) -> Self {
        Self { state, terminal }
    }

    pub async fn run(&mut self) -> miette::Result<S::Output> {
        loop {
            // VIEW: render current state
            self.terminal.draw(|frame| self.state.view(frame)).into_diagnostic()?;

            // EVENT: collect input
            if event::poll(Duration::from_millis(50)).into_diagnostic()? {
                let event = event::read().into_diagnostic()?;
                // UPDATE: produce new state + optional side-effect
                let action = self.state.update(event)?;
                match action {
                    Action::Continue => {}
                    Action::Quit(output) => return Ok(output),
                }
            }

            // TICK: update animations (spinners, etc.)
            self.state.tick();
        }
    }
}

pub async fn dispatch(
    command: Option<Commands>,
    output_mode: &mut OutputMode,
    update_status: std::sync::Arc<std::sync::Mutex<Option<String>>>,
) -> miette::Result<()> {
    match command {
        Some(Commands::New { project_name, library }) => {
            commands::new::run(project_name, library, output_mode, update_status).await?;
        }
        Some(Commands::Build { watch, skip_lock_check }) => {
            commands::build::run(watch, skip_lock_check, output_mode).await?;
        }
        Some(Commands::Run { watch }) => {
            commands::run::run(watch, output_mode).await?;
        }
        Some(Commands::Test { watch }) => {
            commands::test::run(watch, output_mode).await?;
        }
        Some(Commands::Lock(args)) => {
            commands::lock::run(args, output_mode).await?;
        }
        Some(Commands::Ide { host, port }) => {
            commands::ide::run(host, port, output_mode).await?;
        }
        Some(Commands::Inspect { subcommand }) => {
            commands::inspect::run(subcommand)?;
        }
        Some(Commands::Validate { path, json }) => {
            commands::validate::run(path, json)?;
        }
        Some(Commands::Skills { subcommand }) => {
            commands::skills::run(subcommand, output_mode).await?;
        }
        None => {
            let _ = output_mode;
            println!("The NeoHaskell CLI. Run `neo --help` for commands.");
        }
    }
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use crate::test_utils::tests::TEST_MUTEX;

    struct DummyState {
        ticks: u32,
        received_enter: bool,
    }

    impl State for DummyState {
        type Output = bool;

        fn view(&self, _frame: &mut Frame) {}

        fn update(&mut self, event: Event) -> miette::Result<Action<Self::Output>> {
            if let Event::Key(key) = event {
                if key.code == KeyCode::Enter {
                    self.received_enter = true;
                    return Ok(Action::Quit(true));
                }
                if key.code == KeyCode::Esc {
                    return Ok(Action::Quit(false));
                }
            }
            Ok(Action::Continue)
        }

        fn tick(&mut self) {
            self.ticks += 1;
        }
    }

    #[test]
    fn test_dummy_state_update() {
        let mut state = DummyState { ticks: 0, received_enter: false };
        let event = Event::Key(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
        let action = state.update(event.clone()).unwrap();
        match action {
            Action::Quit(res) => assert!(res),
            _ => panic!("Expected Quit"),
        }
        assert!(state.received_enter);
    }
    
    #[tokio::test]
    async fn test_dispatch_none() {
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        // This should run without error and just print the fallback message
        let result = dispatch(None, &mut output_mode, update_status).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_dispatch_new() {
        let _lock = TEST_MUTEX.lock().unwrap();
        let temp = tempfile::tempdir().unwrap();
        let project_dir = temp.path().join("dispatch-project");
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();
        
        unsafe { std::env::set_var("NEO_SKIP_NETWORK", "1"); }
        
        let command = Some(Commands::New { project_name: Some("dispatch-project".to_string()), library: false });
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        
        let result = dispatch(command, &mut output_mode, update_status).await;
        
        std::env::set_current_dir(original_dir).unwrap();
        
        assert!(result.is_ok());
        assert!(project_dir.exists());
        assert!(project_dir.join("neo.json").exists());
    }

    #[tokio::test]
    async fn test_dispatch_build() {
        let _lock = TEST_MUTEX.lock().unwrap();
        let temp = tempfile::tempdir().unwrap();
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();
        
        // Should fail without neo.json
        let command = Some(Commands::Build { watch: false, skip_lock_check: false });
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        let result = dispatch(command, &mut output_mode, update_status).await;
        assert!(result.is_err());
        
        std::env::set_current_dir(original_dir).unwrap();
    }

    #[tokio::test]
    async fn test_dispatch_run() {
        let _lock = TEST_MUTEX.lock().unwrap();
        let temp = tempfile::tempdir().unwrap();
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();
        
        let command = Some(Commands::Run { watch: false });
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        let result = dispatch(command, &mut output_mode, update_status).await;
        assert!(result.is_err());
        
        std::env::set_current_dir(original_dir).unwrap();
    }

    #[tokio::test]
    async fn test_dispatch_test() {
        let _lock = TEST_MUTEX.lock().unwrap();
        let temp = tempfile::tempdir().unwrap();
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();
        
        let command = Some(Commands::Test { watch: false });
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        let result = dispatch(command, &mut output_mode, update_status).await;
        assert!(result.is_err());
        
        std::env::set_current_dir(original_dir).unwrap();
    }

    #[tokio::test]
    async fn test_dispatch_ide_port_in_use_errors_actionably() {
        // Occupy a port, then ask `neo ide` to bind to the same address.
        // dispatch must return Err with an IdeBind diagnostic — and the
        // error's display + help must name the address and a `--port` fix.
        let probe = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
        let port = probe.local_addr().unwrap().port();
        // Keep `probe` alive across the dispatch call so the port stays bound.

        let host = std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST);
        let command = Some(Commands::Ide { host, port });
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        let result = dispatch(command, &mut output_mode, update_status).await;

        let err = result.expect_err("dispatch must fail when the port is occupied");
        let rendered = format!("{err:?}");
        assert!(
            rendered.contains(&port.to_string()),
            "rendered error should mention the port: {rendered}"
        );
        assert!(
            rendered.contains("--port"),
            "rendered error should suggest --port: {rendered}"
        );

        drop(probe);
    }

    #[tokio::test]
    async fn test_dispatch_skills() {
        let _lock = TEST_MUTEX.lock().unwrap();
        let temp = tempfile::tempdir().unwrap();
        let home = tempfile::tempdir().unwrap();
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();

        let prev_home = std::env::var("NEO_HOME").ok();
        unsafe {
            std::env::set_var("NEO_SKIP_NETWORK", "1");
            std::env::set_var("NEO_HOME", home.path());
        }

        let command = Some(Commands::Skills {
            subcommand: Some(crate::cli::SkillsSubcommand::Setup {
                tools: vec![],
                all_tools: true,
                skills: vec![],
                force: false,
                dry_run: false,
                refresh: false,
                no_primer: false,
            }),
        });
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        let result = dispatch(command, &mut output_mode, update_status).await;

        std::env::set_current_dir(original_dir).unwrap();
        unsafe {
            match prev_home {
                Some(v) => std::env::set_var("NEO_HOME", v),
                None => std::env::remove_var("NEO_HOME"),
            }
        }

        assert!(result.is_ok(), "dispatch skills failed: {result:?}");
        assert!(temp.path().join(".claude/skills/sample-skill/SKILL.md").exists());
        // The primer ships with the offline stub: installed + wired into CLAUDE.md.
        assert!(temp.path().join(".claude/neohaskell.md").exists());
        let claude_md = std::fs::read_to_string(temp.path().join("CLAUDE.md")).unwrap();
        assert!(claude_md.contains("@.claude/neohaskell.md"));
    }

    #[tokio::test]
    async fn test_dispatch_lock() {
        let _lock = TEST_MUTEX.lock().unwrap();
        let temp = tempfile::tempdir().unwrap();
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp.path()).unwrap();
        
        let command = Some(Commands::Lock(crate::cli::LockArgs {
            subcommand: None,
            search: None,
            all: false,
        }));
        let mut output_mode = OutputMode::Ci;
        let update_status = std::sync::Arc::new(std::sync::Mutex::new(None));
        let result = dispatch(command, &mut output_mode, update_status).await;
        assert!(result.is_ok());
        
        std::env::set_current_dir(original_dir).unwrap();
    }
}
