use std::path::Path;
use walkdir::WalkDir;

/// Discovers Haskell modules in the given source directory.
pub fn discover<P: AsRef<Path>>(src_dir: P) -> Vec<String> {
    let src_dir = src_dir.as_ref();
    let mut modules = Vec::new();

    for entry in WalkDir::new(src_dir).into_iter().filter_map(|e| e.ok()) {
        if entry.file_type().is_file() {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("hs") {
                if let Ok(relative_path) = path.strip_prefix(src_dir) {
                    let mut components: Vec<String> = relative_path
                        .components()
                        .filter_map(|c| c.as_os_str().to_str().map(|s| s.to_string()))
                        .collect();
                    
                    if let Some(last) = components.last_mut() {
                        let stem = Path::new(last)
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .map(|s| s.to_string());
                        if let Some(s) = stem {
                            *last = s;
                        }
                    }

                    let module_name = components.join(".");
                    // Exclude Main module as it is handled separately
                    if module_name != "Main" {
                        modules.push(module_name);
                    }
                }
            }
        }
    }

    modules.sort();
    modules
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn test_discover_modules() {
        let dir = tempdir().unwrap();
        let src_dir = dir.path().join("src");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(src_dir.join("App")).unwrap();
        fs::create_dir_all(src_dir.join("Domain/Models")).unwrap();

        fs::write(src_dir.join("Main.hs"), "").unwrap();
        fs::write(src_dir.join("Lib.hs"), "").unwrap();
        fs::write(src_dir.join("App/Server.hs"), "").unwrap();
        fs::write(src_dir.join("App/Utils.hs"), "").unwrap();
        fs::write(src_dir.join("Domain/Models/User.hs"), "").unwrap();
        fs::write(src_dir.join("NotHaskell.txt"), "").unwrap();

        let modules = discover(&src_dir);
        assert_eq!(modules, vec!["App.Server", "App.Utils", "Domain.Models.User", "Lib"]);
    }

    #[test]
    fn test_discover_modules_empty() {
        let dir = tempdir().unwrap();
        let src_dir = dir.path().join("src");
        fs::create_dir_all(&src_dir).unwrap();

        let modules = discover(&src_dir);
        assert!(modules.is_empty());
    }
}
