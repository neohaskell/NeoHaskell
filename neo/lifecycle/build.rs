use semver::Version;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let manifest =
        PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect("Cargo sets CARGO_MANIFEST_DIR"))
            .join("..")
            .join("Cargo.toml");
    println!("cargo:rerun-if-changed={}", manifest.display());
    println!("cargo:rerun-if-env-changed=NEO_SOURCE_REVISION");

    let contents = fs::read_to_string(&manifest).expect("read the canonical neo manifest");
    let manifest: toml::Value =
        toml::from_str(&contents).expect("parse the canonical neo manifest");
    let product_version = manifest
        .get("package")
        .and_then(|package| package.get("version"))
        .and_then(toml::Value::as_str)
        .expect("neo/Cargo.toml defines package.version");
    Version::parse(product_version).expect("neo/Cargo.toml package.version is valid SemVer");

    let revision = env::var("NEO_SOURCE_REVISION")
        .ok()
        .filter(|value| {
            matches!(value.len(), 40 | 64) && value.bytes().all(|byte| byte.is_ascii_hexdigit())
        })
        .unwrap_or_else(|| "development".to_owned());
    let target = env::var("TARGET").expect("Cargo sets TARGET for build scripts");

    println!("cargo:rustc-env=NEO_PRODUCT_VERSION={product_version}");
    println!("cargo:rustc-env=NEO_SOURCE_REVISION={revision}");
    println!("cargo:rustc-env=NEO_BUILD_TARGET={target}");
}
