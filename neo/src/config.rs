use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::errors::NeoError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum ProjectKind {
    #[default]
    Executable,
    Library,
}

impl ProjectKind {
    pub fn is_library(self) -> bool {
        matches!(self, ProjectKind::Library)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "kebab-case")]
pub struct NeoConfig {
    pub name: String,
    pub version: String,
    pub neo_version: String,
    pub description: Option<String>,
    pub author: Option<String>,
    #[serde(default = "default_license")]
    pub license: String,
    #[serde(rename = "type", default)]
    pub kind: ProjectKind,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,

    /// Path the config was loaded from, for diagnostic source spans.
    /// `None` when the config was constructed programmatically (tests, defaults).
    #[serde(skip)]
    pub source_path: Option<String>,
    /// Raw file content the config was parsed from. Used by dependency-error
    /// diagnostics to underline the offending entry in the source.
    #[serde(skip)]
    pub source_content: Option<String>,
}

fn default_license() -> String {
    "Apache-2.0".to_string()
}

impl NeoConfig {
    pub fn load<P: AsRef<Path>>(path: P) -> miette::Result<Self> {
        let path = path.as_ref();
        if !path.exists() {
            return Err(NeoError::NoWorkspace.into());
        }

        let content = std::fs::read_to_string(path)
            .map_err(|e| NeoError::io_at("reading `neo.json`", path, e))?;

        let path_str = path.display().to_string();
        let parsed: Result<NeoConfig, _> = serde_json::from_str(&content);
        match parsed {
            Ok(mut config) => {
                config.source_path = Some(path_str);
                config.source_content = Some(content);
                Ok(config)
            }
            Err(e) => {
                let line = e.line();
                let col = e.column();
                let offset = miette::SourceOffset::from_location(&content, line, col);
                Err(NeoError::InvalidConfig {
                    reason: e.to_string(),
                    src: miette::NamedSource::new(path_str, content),
                    bad_bit: miette::SourceSpan::new(offset, 1usize),
                }
                .into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_load_valid_config() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "test-project",
            "version": "0.1.0",
            "neo-version": "main",
            "description": "A test project",
            "author": "NeoHaskell Team",
            "license": "MIT",
            "dependencies": {
                "base": ">= 4.17",
                "aeson": "^>= 2.1"
            }
        }"#;
        file.write_all(json.as_bytes()).unwrap();

        let config = NeoConfig::load(file.path()).unwrap();
        assert_eq!(config.name, "test-project");
        assert_eq!(config.version, "0.1.0");
        assert_eq!(config.neo_version, "main");
        assert_eq!(config.description, Some("A test project".to_string()));
        assert_eq!(config.author, Some("NeoHaskell Team".to_string()));
        assert_eq!(config.license, "MIT");
        assert_eq!(config.dependencies.len(), 2);
        assert_eq!(config.dependencies.get("base").unwrap(), ">= 4.17");
        assert_eq!(config.dependencies.get("aeson").unwrap(), "^>= 2.1");
    }

    #[test]
    fn test_load_default_license() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "test-project",
            "version": "0.1.0",
            "neo-version": "main"
        }"#;
        file.write_all(json.as_bytes()).unwrap();

        let config = NeoConfig::load(file.path()).unwrap();
        assert_eq!(config.license, "Apache-2.0");
        assert!(config.dependencies.is_empty());
    }

    #[test]
    fn test_load_ignore_unknown_fields() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "test-project",
            "version": "0.1.0",
            "neo-version": "main",
            "unknown_field": "some value"
        }"#;
        file.write_all(json.as_bytes()).unwrap();

        let config = NeoConfig::load(file.path()).unwrap();
        assert_eq!(config.name, "test-project");
    }

    #[test]
    fn test_load_missing_file() {
        let result = NeoConfig::load("non_existent.json");
        assert!(result.is_err());
    }

    #[test]
    fn test_load_invalid_json() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{ "name": "test-project", "version": "#;
        file.write_all(json.as_bytes()).unwrap();

        let result = NeoConfig::load(file.path());
        assert!(result.is_err());
    }

    #[test]
    fn test_load_kind_defaults_to_executable() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "p",
            "version": "0.1.0",
            "neo-version": "main"
        }"#;
        file.write_all(json.as_bytes()).unwrap();
        let config = NeoConfig::load(file.path()).unwrap();
        assert_eq!(config.kind, ProjectKind::Executable);
        assert!(!config.kind.is_library());
    }

    #[test]
    fn test_load_kind_library() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "p",
            "version": "0.1.0",
            "neo-version": "main",
            "type": "library"
        }"#;
        file.write_all(json.as_bytes()).unwrap();
        let config = NeoConfig::load(file.path()).unwrap();
        assert_eq!(config.kind, ProjectKind::Library);
        assert!(config.kind.is_library());
    }

    #[test]
    fn test_load_kind_executable_explicit() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "p",
            "version": "0.1.0",
            "neo-version": "main",
            "type": "executable"
        }"#;
        file.write_all(json.as_bytes()).unwrap();
        let config = NeoConfig::load(file.path()).unwrap();
        assert_eq!(config.kind, ProjectKind::Executable);
    }

    #[test]
    fn test_load_kind_invalid_value() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{
            "name": "p",
            "version": "0.1.0",
            "neo-version": "main",
            "type": "not-a-kind"
        }"#;
        file.write_all(json.as_bytes()).unwrap();
        let err = NeoConfig::load(file.path()).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("Invalid") || msg.contains("unknown variant"), "got: {}", msg);
    }

    #[test]
    fn test_load_missing_fields() {
        let mut file = NamedTempFile::new().unwrap();
        let json = r#"{ "name": "test-project" }"#;
        file.write_all(json.as_bytes()).unwrap();

        let result = NeoConfig::load(file.path());
        assert!(result.is_err());
    }
}
