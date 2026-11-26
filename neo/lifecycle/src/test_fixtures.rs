use serde_json::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::LazyLock;

pub(crate) struct CandidateFixture {
    pub(crate) index: Value,
    pub(crate) files: BTreeMap<String, Vec<u8>>,
    pub(crate) runtime_cases: Value,
}

pub(crate) fn current_candidate() -> &'static CandidateFixture {
    static FIXTURE: LazyLock<CandidateFixture> = LazyLock::new(|| {
        if let Some(root) = std::env::var_os("NEO_TEST_FIXTURE_ROOT") {
            return CandidateFixture::load(Path::new(&root));
        }

        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().join("corpus");
        let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let generator = source.join(
            ".agents/skills/neohaskell-release/scripts/releases/candidate_contract_tests/generate_fixtures.py",
        );
        let output = Command::new("python3")
            .arg("-B")
            .arg(&generator)
            .arg("--output-root")
            .arg(&root)
            .output()
            .unwrap_or_else(|error| panic!("run {}: {error}", generator.display()));
        assert!(
            output.status.success(),
            "generate current candidate fixture: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        CandidateFixture::load(&root)
    });
    &FIXTURE
}

impl CandidateFixture {
    fn load(root: &Path) -> Self {
        Self {
            index: read_json(&root.join("fixture-set.json")),
            files: read_fixture_files(&root.join("current")),
            runtime_cases: read_json(&root.join("runtime_cases.json")),
        }
    }
}

fn read_json(path: &Path) -> Value {
    let bytes = fs::read(path).unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    serde_json::from_slice(&bytes)
        .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
}

pub(crate) fn read_fixture_files(directory: &Path) -> BTreeMap<String, Vec<u8>> {
    fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("read {}: {error}", directory.display()))
        .map(|entry| {
            let path = entry.unwrap().path();
            (
                path.file_name().unwrap().to_str().unwrap().into(),
                fs::read(path).unwrap(),
            )
        })
        .collect()
}
