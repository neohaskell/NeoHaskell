use super::*;
use std::fs;
use std::path::{Path, PathBuf};

fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("contract/fixtures/v1")
}
fn cases() -> Value {
    serde_json::from_slice(include_bytes!("../../contract/runtime_cases.json")).unwrap()
}
fn files(directory: &Path) -> BTreeMap<String, Vec<u8>> {
    fs::read_dir(directory)
        .unwrap()
        .map(|entry| {
            let path = entry.unwrap().path();
            (
                path.file_name().unwrap().to_str().unwrap().into(),
                fs::read(path).unwrap(),
            )
        })
        .collect()
}
fn metadata(files: &BTreeMap<String, Vec<u8>>) -> Result<ParsedCandidate> {
    let contract = Contract::load()?;
    let checked = core(&contract, &files["candidate.json"])?;
    let inputs = checked
        .metadata
        .iter()
        .map(|name| {
            Ok((
                name.as_str(),
                files.get(name).ok_or(MetadataError::File)?.as_slice(),
            ))
        })
        .collect::<Result<_>>()?;
    parse(MetadataInputs {
        core: &files["candidate.json"],
        files: inputs,
    })
}

fn apply(directory: &Path, case: &Value) {
    let mut changed_tables = BTreeSet::new();
    for edit in case["edits"].as_array().unwrap() {
        let name = edit["file"].as_str().unwrap();
        let path = directory.join(name);
        let mut raw = fs::read(&path).unwrap();
        let operation = edit["op"].as_str().unwrap();
        match operation {
            "starter-number" => {
                let mut prefix = b"{\"unused_signed_zero\":".to_vec();
                prefix.extend_from_slice(edit["text"].as_str().unwrap().as_bytes());
                prefix.push(b',');
                prefix.extend_from_slice(&raw[1..]);
                raw = prefix;
                let core_path = directory.join("candidate.json");
                let mut core: Value =
                    serde_json::from_slice(&fs::read(&core_path).unwrap()).unwrap();
                core["build_inputs"]["starter"]["lock_sha256"] = hash(&raw).into();
                core["auxiliary_assets"][name]["sha256"] = hash(&raw).into();
                core["auxiliary_assets"][name]["size"] = raw.len().into();
                let receipts_path = directory.join("build-receipts.json");
                let mut receipts: Value =
                    serde_json::from_slice(&fs::read(&receipts_path).unwrap()).unwrap();
                for row in receipts["receipts"].as_array_mut().unwrap() {
                    row["build_inputs_sha256"] =
                        hash(&encode(&core["build_inputs"]).unwrap()).into();
                    let role = row["role"].as_str().unwrap();
                    let platform = row["platform"].as_str().unwrap();
                    let entry = match role {
                        "native-neo" => &mut core["native_neo"][platform],
                        "native-installer" => &mut core["native_installers"][platform],
                        _ => &mut core["packages"][platform][role.strip_prefix("nix-").unwrap()],
                    };
                    entry["build_receipt_sha256"] = hash(&encode(row).unwrap()).into();
                }
                fs::write(receipts_path, encode(&receipts).unwrap()).unwrap();
                fs::write(core_path, encode(&core).unwrap()).unwrap();
                changed_tables.insert("build-receipts.json");
            }
            "byteflip" => {
                raw[edit["offset"].as_u64().unwrap() as usize] =
                    edit["byte"].as_u64().unwrap() as u8
            }
            "symlink" => {
                let outside = directory.parent().unwrap().join(format!("outside-{name}"));
                fs::write(&outside, &raw).unwrap();
                fs::remove_file(&path).unwrap();
                #[cfg(unix)]
                std::os::unix::fs::symlink(outside, &path).unwrap();
                #[cfg(windows)]
                std::os::windows::fs::symlink_file(outside, &path).unwrap();
                continue;
            }
            "suffix" => raw.extend_from_slice(edit["text"].as_str().unwrap().as_bytes()),
            "replace-prefix" => raw
                .splice(..1, edit["text"].as_str().unwrap().bytes())
                .for_each(drop),
            _ => {
                let mut value: Value = serde_json::from_slice(&raw).unwrap();
                let pointer = edit["pointer"].as_str().unwrap();
                let (parent, last) = pointer.rsplit_once('/').unwrap();
                let target = value.pointer_mut(parent).unwrap();
                if let Some(list) = target.as_array_mut() {
                    if operation == "remove" {
                        list.remove(last.parse::<usize>().unwrap());
                    } else if last == "-" {
                        list.push(edit["value"].clone());
                    } else {
                        list[last.parse::<usize>().unwrap()] = edit["value"].clone();
                    }
                } else {
                    let map = target.as_object_mut().unwrap();
                    if operation == "remove" {
                        map.remove(last);
                    } else {
                        map.insert(last.into(), edit["value"].clone());
                    }
                }
                raw = encode(&value).unwrap();
                if name == "closures.json" || name == "build-receipts.json" {
                    changed_tables.insert(name);
                }
            }
        }
        fs::write(path, raw).unwrap();
    }
    if !changed_tables.is_empty() {
        let path = directory.join("candidate.json");
        let mut value: Value = serde_json::from_slice(&fs::read(&path).unwrap()).unwrap();
        for name in changed_tables {
            let raw = fs::read(directory.join(name)).unwrap();
            let field = if name == "closures.json" {
                "closures"
            } else {
                "build_receipts"
            };
            value[field]["sha256"] = hash(&raw).into();
            value[field]["size"] = raw.len().into();
        }
        fs::write(path, encode(&value).unwrap()).unwrap();
    }
}

#[test]
fn frozen_candidates_project_only_the_selected_closure() {
    let index: Value = serde_json::from_slice(include_bytes!(
        "../../contract/fixtures/v1/fixture-set.json"
    ))
    .unwrap();
    let contract = Contract::load().unwrap();
    for (name, id) in index["candidate_ids"].as_object().unwrap() {
        let directory = fixture_root().join(name);
        let loaded = files(&directory);
        let parsed = metadata(&loaded).unwrap();
        assert_eq!(parsed.id(), id.as_str().unwrap());
        assert_eq!(parsed.version(), name.split_once('-').unwrap().1);
        for system in contract.registry()["roles"]["nix_packages"]["systems"]
            .as_array()
            .unwrap()
        {
            let selected = parsed.selected(system.as_str().unwrap()).unwrap();
            for role in contract.registry()["roles"]["nix_packages"]["outputs"]
                .as_array()
                .unwrap()
            {
                assert!(
                    selected
                        .closure()
                        .contains_key(selected.output(role.as_str().unwrap()).unwrap())
                );
            }
            for item in selected.closure().values() {
                assert!(item.hash().starts_with("sha256-"));
                assert!(item.size() > 0);
                assert!(
                    item.references()
                        .iter()
                        .all(|path| selected.closure().contains_key(path))
                );
            }
            assert!(
                selected.closure().len()
                    < index["closure_shape"]["paths"].as_u64().unwrap() as usize
            );
        }
        assert_eq!(
            parsed.selected("foreign-system").unwrap_err(),
            MetadataError::System
        );
    }
}

#[test]
fn shared_canonical_vectors_match_python() {
    let contract = Contract::load().unwrap();
    for vector in cases()["canonical_vectors"].as_array().unwrap() {
        let raw = vector["raw"].as_str().unwrap().as_bytes();
        assert_eq!(
            contract.decode(raw, "max_core_bytes", false, true).is_ok(),
            vector["valid"].as_bool().unwrap(),
            "{vector}"
        );
    }
    assert_eq!(
        decode(b"[0,0]", 5, 4, 2, false, true).unwrap_err(),
        MetadataError::Json
    );
    assert_eq!(
        decode(b"[0,0]", 4, 4, 3, false, true).unwrap_err(),
        MetadataError::Bounds
    );
}

#[test]
fn shared_integer_vectors_match_python() {
    let contract = Contract::load().unwrap();
    for vector in cases()["integer_vectors"].as_array().unwrap() {
        assert_eq!(
            contract
                .decode(
                    vector["raw"].as_str().unwrap().as_bytes(),
                    "max_core_bytes",
                    true,
                    false
                )
                .is_ok(),
            vector["valid"].as_bool().unwrap(),
            "{vector}"
        );
    }
    assert_eq!(
        decode(b"-0", 2, 4, 1, false, true).unwrap_err(),
        MetadataError::Canonical
    );
    assert_eq!(
        decode(b"[-0,-0]", 7, 4, 2, false, false).unwrap_err(),
        MetadataError::Json
    );
    assert_eq!(
        decode(b"[[-0]]", 6, 1, 3, false, false).unwrap_err(),
        MetadataError::Json
    );
}

#[test]
fn shared_cases_validate_metadata_independently_of_payload_files() {
    for case in cases()["cases"].as_array().unwrap() {
        let temporary = tempfile::tempdir().unwrap();
        let directory = temporary.path().join("candidate");
        fs::create_dir(&directory).unwrap();
        for (name, raw) in files(&fixture_root().join("current-0.17.2")) {
            fs::write(directory.join(name), raw).unwrap();
        }
        apply(&directory, case);
        let loaded = files(&directory);
        let result = metadata(&loaded);
        if let Some(expected) = case["error"].as_str() {
            assert_eq!(
                format!("{:?}", result.as_ref().unwrap_err()),
                expected,
                "{}",
                case["name"]
            );
        }
        assert_eq!(
            result.is_ok(),
            case["metadata_valid"].as_bool().unwrap(),
            "metadata {}",
            case["name"]
        );
    }
}

#[test]
fn generated_export_manifest_binds_exact_runtime_bytes() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("contract");
    let manifest: Value =
        serde_json::from_slice(include_bytes!("../../contract/manifest.json")).unwrap();
    assert_eq!(manifest["contract_registry_sha256"], hash(REGISTRY));
    for (name, identity) in manifest["outputs"].as_object().unwrap() {
        let raw = fs::read(root.join(name)).unwrap();
        assert_eq!(identity["sha256"], hash(&raw));
        assert_eq!(identity["size"], raw.len());
    }
}

#[test]
fn versions_and_paths_preserve_the_producer_grammar() {
    for valid in [
        "0.0.0",
        "0.18446744073709551616.0",
        "0.1.999999999999999999999999999999",
    ] {
        assert_eq!(version(&json!(valid)).unwrap(), valid);
    }
    for invalid in [
        "1.0.0",
        "0.01.2",
        "0.1.2+build",
        "0.1.2-rc",
        "0.1.2.3",
        "0.1.-2",
        "0.1.٢",
    ] {
        assert!(version(&json!(invalid)).is_err());
    }
    let contract = Contract::load().unwrap();
    for invalid in [
        "/nix/store/00000000000000000000000000000000-name.drv",
        "/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-name",
        "/nix/store/00000000000000000000000000000000-name/bin",
        "/nix/store/00000000000000000000000000000000-..",
        "/nix/store/00000000000000000000000000000000-name^out",
    ] {
        assert!(contract.path(&json!(invalid)).is_err());
    }
}

#[test]
fn output_paths_preserve_the_producer_store_name_grammar() {
    let contract = Contract::load().unwrap();
    let prefix = "/nix/store/00000000000000000000000000000000-";
    let maximum = format!("{prefix}{}", "n".repeat(211));
    let too_long = format!("{prefix}{}", "n".repeat(212));
    let valid = [format!("{prefix}.hidden"), maximum];
    let invalid = [
        "/tmp/output".to_owned(),
        format!("/nix/store/{}-name", "0".repeat(31)),
        format!("/nix/store/{}-name", "e".repeat(32)),
        prefix.to_owned(),
        format!("{prefix}."),
        format!("{prefix}.."),
        format!("{prefix}name.drv"),
        format!("{prefix}name/bin/neo"),
        format!("{prefix}name^out"),
        format!("{prefix}name\n"),
        format!("{prefix}namé"),
        too_long,
    ];

    for path in valid {
        let constructed = OutputPath::parse(&path).unwrap();
        assert_eq!(constructed.as_str(), path);
        assert_eq!(contract.path(&json!(path)).unwrap(), constructed);
    }
    for path in invalid {
        assert!(OutputPath::parse(&path).is_err(), "{path}");
        assert!(contract.path(&json!(path)).is_err(), "{path}");
    }
}

#[test]
fn frozen_witness_checks_every_signature_against_its_assigned_key() {
    use ring::signature::KeyPair;
    let key = ring::signature::Ed25519KeyPair::from_seed_unchecked(&[7; 32]).unwrap();
    let other = ring::signature::Ed25519KeyPair::from_seed_unchecked(&[8; 32]).unwrap();
    let path = OutputPath::parse("/nix/store/00000000000000000000000000000000-pair").unwrap();
    let nix32 = "0".repeat(52);
    let fingerprint = format!("1;{};sha256:{};1;", path.as_str(), nix32);
    let signature: [u8; 64] = key
        .sign(fingerprint.as_bytes())
        .as_ref()
        .try_into()
        .unwrap();
    let metadata = NarMetadata {
        hash: format!("sha256-{}", STANDARD.encode([0; 32])),
        size: 1,
        references: vec![],
        locations: vec![],
    };
    let mut witness = CacheWitness {
        cache: DeclaredCache {
            url: "https://example.invalid".into(),
            key: key.public_key().as_ref().try_into().unwrap(),
        },
        signatures: vec![signature],
    };
    witness.verify(&path, &metadata, &nix32).unwrap();
    witness.signatures.push(
        other
            .sign(fingerprint.as_bytes())
            .as_ref()
            .try_into()
            .unwrap(),
    );
    assert!(witness.verify(&path, &metadata, &nix32).is_err());
    witness.signatures.pop();
    witness.cache.key = other.public_key().as_ref().try_into().unwrap();
    assert!(witness.verify(&path, &metadata, &nix32).is_err());
    witness.cache.key = key.public_key().as_ref().try_into().unwrap();
    witness.signatures[0][0] ^= 1;
    assert!(witness.verify(&path, &metadata, &nix32).is_err());
}
