use minijinja::{context, Environment};
use std::fs;
use crate::reconcile::resolve::{ResolvedConfig, DependencySource};
use crate::errors::NeoError;

use std::path::Path;

pub fn generate<P: AsRef<Path>>(
    project_dir: P,
    env: &Environment,
    config: &ResolvedConfig,
    modules: &[String],
    test_modules: &[String],
) -> miette::Result<()> {
    let template = env.get_template("project.cabal")
        .map_err(|e| NeoError::TemplateError { template: "project.cabal".to_string(), reason: e.to_string() })?;

    let dependencies: Vec<(String, String)> = config.dependencies.iter().map(|dep| {
        let version = match &dep.source {
            DependencySource::Hackage(v) => v.clone(),
            _ => ">= 0".to_string(), // For git/file, we just need a valid constraint
        };
        (dep.name.clone(), version)
    }).collect();

    let rendered = template.render(context! {
        name => config.name,
        version => config.version,
        description => config.description,
        license => config.license,
        author => config.author,
        modules => modules,
        dependencies => dependencies,
        is_library => config.kind.is_library(),
        test_modules => test_modules,
    }).map_err(|e| NeoError::TemplateError { template: "project.cabal".to_string(), reason: e.to_string() })?;

    let filename = format!("{}.cabal", config.name);
    let out_path = project_dir.as_ref().join(filename);
    fs::write(&out_path, rendered).map_err(|e| NeoError::io_at("writing generated cabal file at", &out_path, e))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    use crate::reconcile::resolve::{ResolvedConfig, ResolvedDependency, DependencySource};

    fn rc(deps: Vec<ResolvedDependency>) -> ResolvedConfig {
        ResolvedConfig {
            name: "p".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            neo_sha: "abc".to_string(),
            description: None,
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: deps,
        }
    }

    fn dep_env() -> Environment<'static> {
        let mut env = Environment::new();
        env.add_template(
            "project.cabal",
            "name: {{name}}\nbuild-depends: base\n{% for dep, ver in dependencies %}    , {{dep}} {{ver}}\n{% endfor %}",
        ).unwrap();
        env
    }

    #[test]
    fn cabal_emits_hackage_dep_with_constraint() {
        let dir = tempdir().unwrap();
        let env = dep_env();
        let mut config = rc(vec![
            ResolvedDependency {
                name: "aeson".to_string(),
                source: DependencySource::Hackage(">=2.0 && <3.0".to_string()),
            },
        ]);
        config.name = "test-deps".to_string();
        generate(dir.path(), &env, &config, &[], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("test-deps.cabal")).unwrap();
        assert!(content.contains(", aeson >=2.0 && <3.0"), "got: {}", content);
    }

    #[test]
    fn cabal_emits_hackage_dep_empty_constraint() {
        let dir = tempdir().unwrap();
        let env = dep_env();
        let mut config = rc(vec![
            ResolvedDependency {
                name: "base".to_string(),
                source: DependencySource::Hackage(String::new()),
            },
        ]);
        config.name = "test-empty".to_string();
        generate(dir.path(), &env, &config, &[], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("test-empty.cabal")).unwrap();
        // Empty version: line ends with the name and a trailing space (no constraint).
        assert!(content.contains(", base "), "got: {}", content);
    }

    #[test]
    fn cabal_emits_or_range_parenthesized() {
        let dir = tempdir().unwrap();
        let env = dep_env();
        let mut config = rc(vec![
            ResolvedDependency {
                name: "foo".to_string(),
                source: DependencySource::Hackage("(>=1.0.0 && <2.0.0) || (>=3.0.0)".to_string()),
            },
        ]);
        config.name = "test-or".to_string();
        generate(dir.path(), &env, &config, &[], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("test-or.cabal")).unwrap();
        assert!(content.contains("(>=1.0.0 && <2.0.0) || (>=3.0.0)"), "got: {}", content);
    }

    #[test]
    fn cabal_omits_git_deps_from_build_depends_constraint() {
        let dir = tempdir().unwrap();
        let env = dep_env();
        let mut config = rc(vec![
            ResolvedDependency {
                name: "my-git-pkg".to_string(),
                source: DependencySource::Git {
                    url: "https://github.com/me/g".to_string(),
                    rev: "main".to_string(),
                },
            },
            ResolvedDependency {
                name: "my-file-pkg".to_string(),
                source: DependencySource::File("../local".to_string()),
            },
        ]);
        config.name = "test-non-hackage".to_string();
        generate(dir.path(), &env, &config, &[], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("test-non-hackage.cabal")).unwrap();
        // Both packages appear with a placeholder constraint, not as Hackage versions.
        assert!(content.contains(", my-git-pkg >= 0"), "got: {}", content);
        assert!(content.contains(", my-file-pkg >= 0"), "got: {}", content);
    }

    #[test]
    fn cabal_no_dependencies() {
        let dir = tempdir().unwrap();
        let env = dep_env();
        let mut config = rc(vec![]);
        config.name = "test-empty-deps".to_string();
        generate(dir.path(), &env, &config, &[], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("test-empty-deps.cabal")).unwrap();
        assert!(content.contains("build-depends: base"));
    }

    #[test]
    fn cabal_executable_stanza_by_default() {
        // Default (Executable) projects keep the `executable <name>` stanza so
        // `cabal build` can produce the launcher binary.
        let dir = tempdir().unwrap();
        let mut env = Environment::new();
        env.add_template(
            "project.cabal",
            include_str!("../../assets/templates/project.cabal.j2"),
        ).unwrap();

        let mut config = rc(vec![]);
        config.name = "test-exec".to_string();
        // kind defaults to Executable in rc()

        generate(dir.path(), &env, &config, &["App".to_string()], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("test-exec.cabal")).unwrap();
        assert!(content.contains("executable test-exec"), "missing executable stanza:\n{}", content);
        assert!(content.contains("main-is: Launcher.hs"), "missing main-is:\n{}", content);
        assert!(content.contains("hs-source-dirs: launcher"), "missing launcher source dir:\n{}", content);
    }

    #[test]
    fn cabal_omits_executable_stanza_for_library() {
        // `type: library` projects have no launcher → the generated cabal file
        // must NOT contain the `executable <name>` stanza, `main-is: Launcher.hs`,
        // or the `hs-source-dirs: launcher` line. The library stanza stays.
        let dir = tempdir().unwrap();
        let mut env = Environment::new();
        env.add_template(
            "project.cabal",
            include_str!("../../assets/templates/project.cabal.j2"),
        ).unwrap();

        let mut config = rc(vec![]);
        config.name = "my-lib".to_string();
        config.kind = crate::config::ProjectKind::Library;

        generate(dir.path(), &env, &config, &["MyLib".to_string()], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("my-lib.cabal")).unwrap();
        assert!(!content.contains("executable my-lib"), "library project should not declare an executable:\n{}", content);
        assert!(!content.contains("main-is: Launcher.hs"), "library project should not reference Launcher.hs:\n{}", content);
        assert!(!content.contains("hs-source-dirs: launcher"), "library project should not reference launcher dir:\n{}", content);
        // Library stanza is still present
        assert!(content.contains("library"), "library project must keep the library stanza:\n{}", content);
        assert!(content.contains("hs-source-dirs: src"), "library project should still expose src/:\n{}", content);
    }

    #[test]
    fn test_generate_cabal_with_modules() {
        let dir = tempdir().unwrap();

        let mut env = Environment::new();
        env.add_template("project.cabal", "name: {{name}}\nexposed-modules: {% for mod in modules %}{{mod}}{% if not loop.last %}, {% endif %}{% endfor %}").unwrap();

        let config = ResolvedConfig {
            name: "test-modules".to_string(),
            version: "0.1.0".to_string(),
            neo_version: "main".to_string(),
            neo_sha: "abc".to_string(),
            description: None,
            author: None,
            license: "MIT".to_string(),
            kind: crate::config::ProjectKind::Executable,
            dependencies: vec![],
        };

        generate(dir.path(), &env, &config, &["Lib".to_string(), "App.Server".to_string()], &[]).unwrap();

        let content = fs::read_to_string(dir.path().join("test-modules.cabal")).unwrap();
        assert!(content.contains("exposed-modules: Lib, App.Server"));
    }

    fn real_template_env() -> Environment<'static> {
        let mut env = Environment::new();
        env.add_template(
            "project.cabal",
            include_str!("../../assets/templates/project.cabal.j2"),
        ).unwrap();
        env
    }

    #[test]
    fn cabal_emits_test_suite() {
        // Every project gets a `test-suite <name>-test` wired to hspec-discover, the
        // project library, and the standard test deps; every discovered spec is an
        // other-module (so -Wmissing-home-modules under -Werror stays quiet).
        let dir = tempdir().unwrap();
        let env = real_template_env();
        let mut config = rc(vec![]);
        config.name = "with-tests".to_string();
        generate(
            dir.path(),
            &env,
            &config,
            &["App".to_string()],
            &["ExampleSpec".to_string(), "Decider.Order.PlaceSpec".to_string()],
        ).unwrap();
        let content = fs::read_to_string(dir.path().join("with-tests.cabal")).unwrap();
        assert!(content.contains("test-suite with-tests-test"), "missing test-suite:\n{}", content);
        assert!(content.contains("type: exitcode-stdio-1.0"), "missing type:\n{}", content);
        assert!(content.contains("main-is: Spec.hs"), "missing main-is:\n{}", content);
        assert!(content.contains("hs-source-dirs: tests"), "missing test source dir:\n{}", content);
        // hspec-discover is a PATH tool; declaring the exe as a build-tool-depends
        // makes cabal's solver reject the installed library (Cabal-7107), so the
        // `hspec-discover:hspec-discover` build-tool entry must NOT be emitted.
        assert!(!content.contains("hspec-discover:hspec-discover"), "must not declare hspec-discover build-tool:\n{}", content);
        assert!(content.contains("hspec"), "missing hspec dep:\n{}", content);
        assert!(content.contains("QuickCheck"), "missing QuickCheck dep:\n{}", content);
        assert!(content.contains("quickcheck-instances"), "missing quickcheck-instances dep:\n{}", content);
        // The project's own library is a test dependency so `import App` resolves.
        assert!(content.contains("with-tests,"), "missing project-library dep:\n{}", content);
        // Discovered specs are listed as other-modules.
        assert!(content.contains("ExampleSpec"), "missing ExampleSpec other-module:\n{}", content);
        assert!(content.contains("Decider.Order.PlaceSpec"), "missing nested other-module:\n{}", content);
    }

    #[test]
    fn cabal_test_suite_present_for_library_projects_too() {
        // Library projects have no executable but should still be testable.
        let dir = tempdir().unwrap();
        let env = real_template_env();
        let mut config = rc(vec![]);
        config.name = "lib-with-tests".to_string();
        config.kind = crate::config::ProjectKind::Library;
        generate(
            dir.path(),
            &env,
            &config,
            &["MyLib".to_string()],
            &["ExampleSpec".to_string()],
        ).unwrap();
        let content = fs::read_to_string(dir.path().join("lib-with-tests.cabal")).unwrap();
        assert!(!content.contains("executable lib-with-tests"), "library must not declare executable:\n{}", content);
        assert!(content.contains("test-suite lib-with-tests-test"), "library must still get a test-suite:\n{}", content);
    }

    #[test]
    fn cabal_test_suite_omits_other_modules_when_only_driver() {
        // test/ with only the Spec.hs driver → test_modules is empty → no dangling
        // `other-modules:` header. hspec-discover finds no specs and the suite passes.
        let dir = tempdir().unwrap();
        let env = real_template_env();
        let mut config = rc(vec![]);
        config.name = "driver-only".to_string();
        generate(dir.path(), &env, &config, &["App".to_string()], &[]).unwrap();
        let content = fs::read_to_string(dir.path().join("driver-only.cabal")).unwrap();
        assert!(content.contains("test-suite driver-only-test"), "missing test-suite:\n{}", content);
        assert!(!content.contains("other-modules:"), "unexpected empty other-modules header:\n{}", content);
    }
}
