use crate::app::{Action, State};
use crate::config::ProjectKind;
use crate::output::OutputMode;
use crate::theme::Theme;
use crate::tui::banner::Banner;
use crate::tui::prompt::Prompt;
use crate::tui::selection::Selection;
use miette::IntoDiagnostic;
use ratatui::{
    crossterm::event::{Event, KeyCode},
    layout::{Constraint, Direction, Layout},
    Frame,
};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub neo_version: String,
    pub description: String,
    pub author: String,
    pub license: String,
    #[serde(rename = "type", default, skip_serializing_if = "is_executable")]
    pub kind: ProjectKind,
}

fn is_executable(k: &ProjectKind) -> bool {
    matches!(k, ProjectKind::Executable)
}

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            name: "my-neo-app".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            description: "A new NeoHaskell project".to_string(),
            author: "Anonymous".to_string(),
            license: "MIT".to_string(),
            kind: ProjectKind::Executable,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Step {
    Name,
    Version,
    Description,
    Author,
    License,
}

use crate::tui::footer::Footer;
use std::sync::{Arc, Mutex};

pub struct NewProjectState {
    theme: Theme,
    config: ProjectConfig,
    current_step: Step,
    input_buffer: String,
    cursor_visible: bool,
    tick_count: u32,
    licenses: Vec<&'static str>,
    selected_license: usize,
    update_status: Arc<Mutex<Option<String>>>,
}

impl NewProjectState {
    pub fn new(
        theme: Theme,
        initial_name: Option<String>,
        update_status: Arc<Mutex<Option<String>>>,
    ) -> Self {
        let mut config = ProjectConfig::default();
        if let Some(name) = initial_name {
            config.name = name;
        }

        let input_buffer = config.name.clone();

        Self {
            theme,
            config,
            current_step: Step::Name,
            input_buffer,
            cursor_visible: true,
            tick_count: 0,
            licenses: vec!["MIT", "Apache-2.0", "BSD-3-Clause", "GPL-3.0", "None"],
            selected_license: 0,
            update_status,
        }
    }
}

impl State for NewProjectState {
    type Output = ProjectConfig;

    fn view(&self, frame: &mut Frame) {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(12), // Banner
                Constraint::Min(0),      // Content
                Constraint::Length(2),  // Footer
            ])
            .split(frame.area());

        let banner = Banner::new(&self.theme, "NEO", "The NeoHaskell CLI").with_frame(self.tick_count as usize);
        frame.render_widget(banner, chunks[0]);

        let content_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(4), // Prompt
                Constraint::Min(0),    // Footer/Hints
            ])
            .split(chunks[1]);

        let update_available = self.update_status.lock().unwrap();
        let footer = Footer::new(
            &self.theme,
            env!("CARGO_PKG_VERSION"),
            update_available.as_deref(),
        );
        frame.render_widget(footer, chunks[2]);

        match self.current_step {
            Step::Name => {
                let prompt = Prompt::new(
                    &self.theme,
                    "What is the name of your project?",
                    &self.input_buffer,
                    "my-neo-app",
                    self.cursor_visible,
                );
                frame.render_widget(prompt, content_chunks[0]);
            }
            Step::Version => {
                let prompt = Prompt::new(
                    &self.theme,
                    "Project version?",
                    &self.input_buffer,
                    "0.1.0",
                    self.cursor_visible,
                );
                frame.render_widget(prompt, content_chunks[0]);
            }
            Step::Description => {
                let prompt = Prompt::new(
                    &self.theme,
                    "Short description?",
                    &self.input_buffer,
                    "A new NeoHaskell project",
                    self.cursor_visible,
                );
                frame.render_widget(prompt, content_chunks[0]);
            }
            Step::Author => {
                let prompt = Prompt::new(
                    &self.theme,
                    "Author name?",
                    &self.input_buffer,
                    "Anonymous",
                    self.cursor_visible,
                );
                frame.render_widget(prompt, content_chunks[0]);
            }
            Step::License => {
                let selection = Selection::new(
                    &self.theme,
                    "Choose a license:",
                    &self.licenses,
                    self.selected_license,
                );
                frame.render_widget(selection, content_chunks[0]);
            }
        }
    }

    fn update(&mut self, event: Event) -> miette::Result<Action<Self::Output>> {
        if let Event::Key(key) = event {
            match key.code {
                KeyCode::Enter => {
                    match self.current_step {
                        Step::Name => {
                            if !self.input_buffer.is_empty() {
                                self.config.name = self.input_buffer.clone();
                            }
                            self.current_step = Step::Version;
                            self.input_buffer = self.config.version.clone();
                        }
                        Step::Version => {
                            if !self.input_buffer.is_empty() {
                                self.config.version = self.input_buffer.clone();
                            }
                            self.current_step = Step::Description;
                            self.input_buffer = self.config.description.clone();
                        }
                        Step::Description => {
                            if !self.input_buffer.is_empty() {
                                self.config.description = self.input_buffer.clone();
                            }
                            self.current_step = Step::Author;
                            self.input_buffer = self.config.author.clone();
                        }
                        Step::Author => {
                            if !self.input_buffer.is_empty() {
                                self.config.author = self.input_buffer.clone();
                            }
                            self.current_step = Step::License;
                        }
                        Step::License => {
                            self.config.license = self.licenses[self.selected_license].to_string();
                            return Ok(Action::Quit(self.config.clone()));
                        }
                    }
                }
                KeyCode::Char(c) => {
                    if self.current_step != Step::License {
                        self.input_buffer.push(c);
                    }
                }
                KeyCode::Backspace => {
                    if self.current_step != Step::License {
                        self.input_buffer.pop();
                    }
                }
                KeyCode::Up => {
                    if self.current_step == Step::License {
                        if self.selected_license > 0 {
                            self.selected_license -= 1;
                        }
                    }
                }
                KeyCode::Down => {
                    if self.current_step == Step::License {
                        if self.selected_license < self.licenses.len() - 1 {
                            self.selected_license += 1;
                        }
                    }
                }
                KeyCode::Esc => {
                    return Ok(Action::Quit(self.config.clone())); // Should probably handle cancel
                }
                _ => {}
            }
        }
        Ok(Action::Continue)
    }

    fn tick(&mut self) {
        self.tick_count += 1;
        if self.tick_count % 10 == 0 {
            self.cursor_visible = !self.cursor_visible;
        }
    }
}

use crate::tui::spinner::Spinner;
use crate::tui::success::SuccessDisplay;

pub async fn run(
    project_name: Option<String>,
    library: bool,
    output_mode: &mut OutputMode,
    update_status: Arc<Mutex<Option<String>>>,
) -> miette::Result<()> {
    let theme = Theme::neo();

    let kind = if library { ProjectKind::Library } else { ProjectKind::Executable };

    let mut config = if matches!(output_mode, OutputMode::Interactive) {
        // Enter alternate screen for the interview
        crossterm::terminal::enable_raw_mode().into_diagnostic()?;
        crossterm::execute!(std::io::stdout(), crossterm::terminal::EnterAlternateScreen)
            .into_diagnostic()?;

        let backend = ratatui::backend::CrosstermBackend::new(std::io::stdout());
        let mut terminal = ratatui::Terminal::new(backend).into_diagnostic()?;

        let state = NewProjectState::new(theme.clone(), project_name, update_status);
        let mut app = crate::app::App::new(state, &mut terminal);
        let result = app.run().await;

        crossterm::execute!(std::io::stdout(), crossterm::terminal::LeaveAlternateScreen)
            .into_diagnostic()?;
        crossterm::terminal::disable_raw_mode().into_diagnostic()?;

        result?
    } else {
        // CI Mode
        ProjectConfig {
            name: project_name.unwrap_or_else(|| "my-neo-app".to_string()),
            ..ProjectConfig::default()
        }
    };

    config.kind = kind;

    scaffold_project(config, output_mode).await?;

    Ok(())
}

async fn do_scaffold(config: ProjectConfig) -> miette::Result<()> {
    let project_path = PathBuf::from(&config.name);
    if project_path.exists() {
        return Err(crate::errors::NeoError::DirectoryExists { name: config.name }.into());
    }

    std::fs::create_dir_all(&project_path)
        .map_err(|e| crate::errors::NeoError::io_at("creating the new project directory at", &project_path, e))?;

    // Nothing existed at this path before this command. If any later scaffold,
    // reconciliation, or git step fails, remove the partial project instead of
    // leaving a directory that looks usable but is incomplete.
    struct RemovePartialProject {
        path: PathBuf,
        armed: bool,
    }
    impl Drop for RemovePartialProject {
        fn drop(&mut self) {
            if self.armed {
                let _ = std::fs::remove_dir_all(&self.path);
            }
        }
    }
    let mut cleanup = RemovePartialProject {
        path: project_path.clone(),
        armed: true,
    };

    // Write neo.json
    let neo_json_path = project_path.join("neo.json");
    let config_json = serde_json::to_string_pretty(&config)
        .map_err(|e| crate::errors::NeoError::io_at(
            "serializing default `neo.json` for",
            &neo_json_path,
            std::io::Error::new(std::io::ErrorKind::InvalidData, e),
        ))?;
    std::fs::write(&neo_json_path, config_json.as_bytes())
        .map_err(|e| crate::errors::NeoError::io_at("writing initial `neo.json` to", &neo_json_path, e))?;

    // Scaffold from the embedded starter template (no network — the starter is
    // baked into the binary, revision-coherent with this monorepo).
    crate::network::write_starter_template(&project_path)?;

    // The starter template ships a `launcher/` directory. Library projects don't
    // need it — remove it after extract so the project tree matches `type: library`.
    if config.kind.is_library() {
        let launcher_dir = project_path.join("launcher");
        if launcher_dir.exists() {
            std::fs::remove_dir_all(&launcher_dir)
                .map_err(|e| crate::errors::NeoError::io_at("removing the starter `launcher/` directory at (library project does not need it)", &launcher_dir, e))?;
        }
    }

    // Ensure launcher/Launcher.hs exists (executable projects only)
    if !config.kind.is_library() {
        let launcher_dir = project_path.join("launcher");
        if !launcher_dir.exists() {
            std::fs::create_dir_all(&launcher_dir)
                .map_err(|e| crate::errors::NeoError::io_at("creating the launcher directory at", &launcher_dir, e))?;
        }
        let launcher_hs = launcher_dir.join("Launcher.hs");
        if !launcher_hs.exists() {
            let launcher_content = format!(
                "module Main where\n\nimport App\n\nmain :: IO ()\nmain = App.run\n"
            );
            std::fs::write(&launcher_hs, launcher_content)
                .map_err(|e| crate::errors::NeoError::io_at("writing `launcher/Launcher.hs` to", &launcher_hs, e))?;
        }
    }

    // Ensure src/App.hs exists (since Launcher.hs imports it for executable projects).
    // Libraries get the same stub so the starter compiles with an `exposed-modules: App` entry.
    let src_app = project_path.join("src/App.hs");
    if !src_app.exists() {
        let app_content = "module App where\n\nrun :: IO ()\nrun = putStrLn \"Hello from NeoHaskell!\"\n";
        std::fs::write(&src_app, app_content)
            .map_err(|e| crate::errors::NeoError::io_at("writing `src/App.hs` to", &src_app, e))?;
    }

    // Scaffold a working Hspec/QuickCheck example so `neo test` compiles and runs a
    // real Haskell test out of the box — but only when the starter shipped no Haskell
    // spec of its own (the starter's `tests/` already holds `.hurl` scenarios, so we
    // gate on the absence of `*Spec.hs`, not the directory). Reconcile (below) writes
    // the `tests/Spec.hs` hspec-discover driver and emits the `test-suite` stanza.
    // Haskell specs and `.hurl` e2e scenarios share the one `tests/` directory.
    if crate::reconcile::test_suite::other_modules(&project_path).is_empty() {
        let test_dir = project_path.join("tests");
        std::fs::create_dir_all(&test_dir)
            .map_err(|e| crate::errors::NeoError::io_at("creating the `tests/` directory at", &test_dir, e))?;
        let example_spec = test_dir.join("ExampleSpec.hs");
        let example_content = "module ExampleSpec (spec) where\n\nimport Core\nimport Test\n\n\nspec :: Spec Unit\nspec = describe \"Example\" do\n  it \"runs a passing test\" \\_ -> do\n    (1 + 1 :: Int) |> shouldBe 2\n";
        std::fs::write(&example_spec, example_content)
            .map_err(|e| crate::errors::NeoError::io_at("writing `tests/ExampleSpec.hs` to", &example_spec, e))?;
    }

    // Run reconciliation to generate .cabal, flake.nix, etc.
    // This ensures these files exist and can be tracked by Git
    let neo_config = crate::config::NeoConfig {
        name: config.name.clone(),
        version: config.version.clone(),
        neo_version: config.neo_version.clone(),
        description: if config.description.is_empty() { None } else { Some(config.description.clone()) },
        author: if config.author.is_empty() { None } else { Some(config.author.clone()) },
        license: config.license.clone(),
        kind: config.kind,
        dependencies: std::collections::HashMap::new(),
        source_path: None,
        source_content: None,
    };
    crate::reconcile::run(&project_path, &neo_config).await?;

    // Safeguard .gitignore: ensure *.cabal is not ignored
    let gitignore_path = project_path.join(".gitignore");
    if gitignore_path.exists() {
        let mut content = std::fs::read_to_string(&gitignore_path)
            .map_err(|e| crate::errors::NeoError::io_at("reading `.gitignore` at", &gitignore_path, e))?;
        if !content.contains("!*.cabal") {
            content.push_str("\n# Ensure cabal files are tracked for Nix\n!*.cabal\n");
            std::fs::write(&gitignore_path, content)
                .map_err(|e| crate::errors::NeoError::io_at("appending to `.gitignore` at", &gitignore_path, e))?;
        }
    }

    // git init
    crate::git::init(&project_path)?;
    crate::git::install_lock_hook(&project_path)?;

    // Write .envrc for direnv/HLS integration
    let envrc_content = "use flake\n";
    let envrc_path = project_path.join(".envrc");
    std::fs::write(&envrc_path, envrc_content)
        .map_err(|e| crate::errors::NeoError::io_at("writing `.envrc` to", &envrc_path, e))?;

    // Add all files to git so Nix flakes can see them
    crate::git::add_all(&project_path)?;
    crate::git::commit(&project_path, "Initial commit from NeoCLI")?;

    cleanup.armed = false;
    Ok(())
}

async fn scaffold_project(config: ProjectConfig, output_mode: &mut OutputMode) -> miette::Result<()> {
    if matches!(output_mode, OutputMode::Interactive) {
        ratatui::crossterm::terminal::enable_raw_mode().unwrap();
        let backend = ratatui::backend::CrosstermBackend::new(std::io::stdout());
        let mut terminal = ratatui::Terminal::with_options(
            backend,
            ratatui::TerminalOptions { viewport: ratatui::Viewport::Inline(13) }
        ).into_diagnostic()?;
        ratatui::crossterm::terminal::disable_raw_mode().unwrap();
        
        let (tx, mut rx) = tokio::sync::mpsc::channel(1);
        let config_clone = config.clone();
        
        tokio::spawn(async move {
            let res = do_scaffold(config_clone).await;
            let _ = tx.send(res).await;
        });

        let mut frame = 0;
        let theme = Theme::neo();
        loop {
            terminal.draw(|f| {
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(12), // Banner
                        Constraint::Min(0),      // Content
                    ])
                    .split(f.area());

                let banner = Banner::new(&theme, "NEO", "Scaffolding Project...").with_frame(frame);
                f.render_widget(banner, chunks[0]);

                let spinner = Spinner::new(&theme, frame).with_label("Downloading template and initializing project...");
                f.render_widget(spinner, chunks[1]);
            }).into_diagnostic()?;

            tokio::select! {
                res = rx.recv() => {
                    match res {
                        Some(Ok(_)) => {
                            // Success animation loop for 2 seconds
                            for i in 0..25 { // 25 * 80ms = 2s
                                terminal.draw(|f| {
                                    let chunks = Layout::default()
                                        .direction(Direction::Vertical)
                                        .constraints([
                                            Constraint::Length(12), // Banner
                                            Constraint::Min(0),      // Content
                                        ])
                                        .split(f.area());

                                    let banner = Banner::new(&theme, "NEO", "Success!").with_frame(frame + i);
                                    f.render_widget(banner, chunks[0]);

                                    let success = SuccessDisplay::new(&theme, "Project created successfully!").with_frame(frame + i);
                                    f.render_widget(success, chunks[1]);
                                }).into_diagnostic()?;
                                tokio::time::sleep(std::time::Duration::from_millis(80)).await;
                            }
                            return Ok(());
                        }
                        Some(Err(e)) => return Err(e),
                        None => return Ok(()),
                    }
                }
                _ = tokio::time::sleep(std::time::Duration::from_millis(80)) => {
                    frame += 1;
                }
            }
        }
    } else {
        // CI Mode
        println!("[info] Creating project {}...", config.name);
        do_scaffold(config).await?;
        println!("[ok] Project created successfully!");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::theme::Theme;
    use ratatui::crossterm::event::{KeyEvent, KeyModifiers};

    #[test]
    fn test_new_project_state_transitions() {
        let theme = Theme::neo();
        let update_status = Arc::new(Mutex::new(None));
        let mut state = NewProjectState::new(theme, Some("my-app".into()), update_status);
        
        assert_eq!(state.current_step, Step::Name);
        assert_eq!(state.input_buffer, "my-app");

        // Transition to Version
        let event = Event::Key(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
        let action = state.update(event.clone()).unwrap();
        assert!(matches!(action, Action::Continue));
        assert_eq!(state.current_step, Step::Version);
        assert_eq!(state.input_buffer, "0.1.0"); // This is project version, which is still 0.1.0

        // Transition to Description
        let action = state.update(event.clone()).unwrap();
        assert!(matches!(action, Action::Continue));
        assert_eq!(state.current_step, Step::Description);

        // Transition to Author
        let action = state.update(event.clone()).unwrap();
        assert!(matches!(action, Action::Continue));
        assert_eq!(state.current_step, Step::Author);

        // Transition to License
        let action = state.update(event.clone()).unwrap();
        assert!(matches!(action, Action::Continue));
        assert_eq!(state.current_step, Step::License);

        // Finalize
        let action = state.update(event).unwrap();
        if let Action::Quit(config) = action {
            assert_eq!(config.name, "my-app");
            assert_eq!(config.license, "MIT");
        } else {
            panic!("Expected Action::Quit");
        }
    }

    #[test]
    fn test_input_handling() {
        let theme = Theme::neo();
        let update_status = Arc::new(Mutex::new(None));
        let mut state = NewProjectState::new(theme, None, update_status);
        state.input_buffer.clear();

        state.update(Event::Key(KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE))).unwrap();
        state.update(Event::Key(KeyEvent::new(KeyCode::Char('b'), KeyModifiers::NONE))).unwrap();
        assert_eq!(state.input_buffer, "ab");

        state.update(Event::Key(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE))).unwrap();
        assert_eq!(state.input_buffer, "a");
    }

    #[test]
    fn test_esc_quits() {
        let theme = Theme::neo();
        let update_status = Arc::new(Mutex::new(None));
        let mut state = NewProjectState::new(theme, None, update_status);
        let action = state.update(Event::Key(KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE))).unwrap();
        assert!(matches!(action, Action::Quit(_)));
    }
}
