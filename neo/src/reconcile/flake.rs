use minijinja::{context, Environment};
use std::fs;
use crate::reconcile::resolve::ResolvedConfig;
use crate::errors::NeoError;

use std::path::Path;

pub fn generate<P: AsRef<Path>>(
    project_dir: P,
    env: &Environment,
    config: &ResolvedConfig,
) -> miette::Result<()> {
    let template = env.get_template("flake.nix")
        .map_err(|e| NeoError::TemplateError { template: "flake.nix".to_string(), reason: e.to_string() })?;

    let rendered = template.render(context! {
        name => config.name,
        description => config.description,
        neo_sha => config.neo_sha,
    }).map_err(|e| NeoError::TemplateError { template: "flake.nix".to_string(), reason: e.to_string() })?;

    let out_path = project_dir.as_ref().join("flake.nix");
    fs::write(&out_path, rendered).map_err(|e| NeoError::io_at("writing generated `flake.nix` at", &out_path, e))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_generate_flake() {
        let dir = tempdir().unwrap();

        let mut env = Environment::new();
        env.add_template("flake.nix", "description: {{description}}, sha: {{neo_sha}}").unwrap();

        let config = ResolvedConfig {
            name: "test-project".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            neo_sha: "abc1234".to_string(),
            description: Some("A test description".to_string()),
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: vec![],
        };

        generate(dir.path(), &env, &config).unwrap();
        
        let content = fs::read_to_string(dir.path().join("flake.nix")).unwrap();
        assert!(content.contains("description: A test description"));
        assert!(content.contains("sha: abc1234"));
    }
}
