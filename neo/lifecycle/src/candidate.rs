//! PARSE-only candidate identity. This module grants no source, signature, or Nix readiness.
//!
//! Refresh the sealed inputs with
//! `python3 .agents/skills/neohaskell-release/scripts/releases/runtime_inputs.py --write`.
//! Use `--check` for a read-only exact export comparison. Cargo never runs Python.

use base64::{Engine, engine::general_purpose::STANDARD};
use serde::de::{self, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

const REGISTRY: &[u8] = include_bytes!("../contract/registry.json");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MetadataError {
    Json,
    Bounds,
    Canonical,
    Shape,
    Identity,
    Graph,
    Receipt,
    Starter,
    File,
    System,
}

type Result<T> = std::result::Result<T, MetadataError>;

/// A lexical output path, not evidence that Nix realized or verified it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct OutputPath(String);

impl OutputPath {
    pub(crate) fn parse(value: &str) -> std::result::Result<Self, MetadataError> {
        Contract::load()?.path(&Value::String(value.to_owned()))
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug)]
pub(crate) struct SelectedOutputs {
    outputs: BTreeMap<String, OutputPath>,
    closure: BTreeMap<OutputPath, NarMetadata>,
}

impl SelectedOutputs {
    pub(crate) fn output(&self, role: &str) -> Option<&OutputPath> {
        self.outputs.get(role)
    }

    pub(crate) fn closure(&self) -> &BTreeMap<OutputPath, NarMetadata> {
        &self.closure
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NarMetadata {
    hash: String,
    size: u64,
    references: Vec<OutputPath>,
    locations: Vec<CacheWitness>,
}

impl NarMetadata {
    pub(crate) fn hash(&self) -> &str {
        &self.hash
    }
    pub(crate) fn size(&self) -> u64 {
        self.size
    }
    pub(crate) fn references(&self) -> &[OutputPath] {
        &self.references
    }
    pub(crate) fn locations(&self) -> &[CacheWitness] {
        &self.locations
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CacheWitness {
    cache: DeclaredCache,
    signatures: Vec<[u8; 64]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DeclaredCache {
    url: String,
    key: [u8; 32],
}

impl DeclaredCache {
    pub(crate) fn url(&self) -> &str {
        &self.url
    }
}

pub(crate) fn declared_cache(index: usize) -> Result<DeclaredCache> {
    let contract = Contract::load()?;
    let entry = array(&contract.registry()["trusted_cache_keys"])?
        .get(index)
        .ok_or(MetadataError::Identity)?;
    let (_, encoded) = string(&entry["key"])?
        .split_once(':')
        .ok_or(MetadataError::Identity)?;
    let key = STANDARD
        .decode(encoded)
        .map_err(|_| MetadataError::Identity)?
        .try_into()
        .map_err(|_| MetadataError::Identity)?;
    Ok(DeclaredCache {
        url: string(&entry["cache"])?.into(),
        key,
    })
}

fn nix_cache_settings() -> Result<(String, String)> {
    let contract = Contract::load()?;
    let entries = array(&contract.registry()["trusted_cache_keys"])?;
    let mut urls = Vec::new();
    let mut keys = Vec::new();
    for entry in entries {
        let url = string(&entry["cache"])?;
        let key = string(&entry["key"])?;
        ensure(
            !url.is_empty()
                && !key.is_empty()
                && !url.contains('?')
                && !url
                    .bytes()
                    .chain(key.bytes())
                    .any(|byte| byte.is_ascii_whitespace() || byte == b'#'),
            MetadataError::Identity,
        )?;
        urls.push(url);
        keys.push(key);
    }
    ensure(!entries.is_empty(), MetadataError::Identity)?;
    Ok((urls.join(" "), keys.join(" ")))
}

pub(crate) fn nix_cache_urls() -> Result<String> {
    nix_cache_settings().map(|(urls, _)| urls)
}

pub(crate) fn nix_cache_policy() -> Result<String> {
    let (urls, keys) = nix_cache_settings()?;
    Ok(format!(
        "extra-experimental-features = nix-command flakes\nextra-substituters = {urls}\nextra-trusted-public-keys = {keys}\n",
    ))
}

impl CacheWitness {
    pub(crate) fn cache(&self) -> &DeclaredCache {
        &self.cache
    }

    pub(crate) fn verify(
        &self,
        path: &OutputPath,
        metadata: &NarMetadata,
        nix32: &str,
    ) -> Result<()> {
        ensure(
            nix32.len() == 52
                && nix32
                    .bytes()
                    .all(|b| b"0123456789abcdfghijklmnpqrsvwxyz".contains(&b)),
            MetadataError::Identity,
        )?;
        let references = metadata
            .references
            .iter()
            .map(OutputPath::as_str)
            .collect::<Vec<_>>()
            .join(",");
        let fingerprint = format!(
            "1;{};sha256:{};{};{}",
            path.as_str(),
            nix32,
            metadata.size,
            references
        );
        let key =
            ring::signature::UnparsedPublicKey::new(&ring::signature::ED25519, self.cache.key);
        for signature in &self.signatures {
            key.verify(fingerprint.as_bytes(), signature)
                .map_err(|_| MetadataError::Identity)?;
        }
        Ok(())
    }
}

fn cache_witness(contract: &Contract, location: &Value) -> Result<CacheWitness> {
    let url = string(&location["cache"])?;
    let entry = array(&contract.registry()["trusted_cache_keys"])?
        .iter()
        .find(|entry| entry["cache"] == url)
        .ok_or(MetadataError::Identity)?;
    let (_, encoded_key) = string(&entry["key"])?
        .split_once(':')
        .ok_or(MetadataError::Identity)?;
    let key = STANDARD
        .decode(encoded_key)
        .map_err(|_| MetadataError::Identity)?
        .try_into()
        .map_err(|_| MetadataError::Identity)?;
    let signatures = array(&location["signatures"])?
        .iter()
        .map(|signature| {
            let (_, encoded) = string(signature)?
                .split_once(':')
                .ok_or(MetadataError::Identity)?;
            STANDARD
                .decode(encoded)
                .map_err(|_| MetadataError::Identity)?
                .try_into()
                .map_err(|_| MetadataError::Identity)
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(CacheWitness {
        cache: DeclaredCache {
            url: url.into(),
            key,
        },
        signatures,
    })
}

#[derive(Debug)]
pub(crate) struct ParsedCandidate {
    id: String,
    version: String,
    systems: BTreeMap<String, SelectedOutputs>,
}

impl ParsedCandidate {
    #[cfg(test)]
    pub(crate) fn fixture_authority(&mut self) {
        use ring::signature::KeyPair;
        let key = ring::signature::Ed25519KeyPair::from_seed_unchecked(&[73; 32]).unwrap();
        for selected in self.systems.values_mut() {
            for metadata in selected.closure.values_mut() {
                for witness in &mut metadata.locations {
                    if witness.cache.url == "https://neohaskell.cachix.org" {
                        witness.cache.key = key.public_key().as_ref().try_into().unwrap();
                    }
                }
            }
        }
    }

    pub(crate) fn id(&self) -> &str {
        &self.id
    }
    pub(crate) fn version(&self) -> &str {
        &self.version
    }
    pub(crate) fn selected(&self, system: &str) -> Result<&SelectedOutputs> {
        self.systems.get(system).ok_or(MetadataError::System)
    }
}

/// Only tables, auxiliary metadata, and bootstrap bytes are required here.
/// Native executables for foreign platforms are never runtime parser inputs.
pub(crate) struct MetadataInputs<'a> {
    pub(crate) core: &'a [u8],
    pub(crate) files: BTreeMap<&'a str, &'a [u8]>,
}

struct DecodeBudget {
    remaining: usize,
    depth: usize,
    booleans: bool,
}

struct Node<'a> {
    budget: &'a mut DecodeBudget,
    depth: usize,
}

impl<'de> DeserializeSeed<'de> for Node<'_> {
    type Value = Value;
    fn deserialize<D: de::Deserializer<'de>>(
        self,
        decoder: D,
    ) -> std::result::Result<Value, D::Error> {
        if self.depth > self.budget.depth || self.budget.remaining == 0 {
            return Err(de::Error::custom("json-bound"));
        }
        self.budget.remaining -= 1;
        decoder.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for Node<'_> {
    type Value = Value;
    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("bounded candidate JSON")
    }
    fn visit_bool<E: de::Error>(self, value: bool) -> std::result::Result<Value, E> {
        if self.budget.booleans {
            Ok(Value::Bool(value))
        } else {
            Err(E::custom("boolean"))
        }
    }
    fn visit_unit<E: de::Error>(self) -> std::result::Result<Value, E> {
        Ok(Value::Null)
    }
    fn visit_i64<E: de::Error>(self, value: i64) -> std::result::Result<Value, E> {
        Ok(value.into())
    }
    fn visit_u64<E: de::Error>(self, value: u64) -> std::result::Result<Value, E> {
        i64::try_from(value)
            .map(Value::from)
            .map_err(|_| E::custom("integer"))
    }
    fn visit_f64<E: de::Error>(self, value: f64) -> std::result::Result<Value, E> {
        if value == 0.0 && value.is_sign_negative() {
            Ok(0.into())
        } else {
            Err(E::custom("float"))
        }
    }
    fn visit_str<E: de::Error>(self, value: &str) -> std::result::Result<Value, E> {
        Ok(value.into())
    }
    fn visit_string<E: de::Error>(self, value: String) -> std::result::Result<Value, E> {
        Ok(value.into())
    }
    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> std::result::Result<Value, A::Error> {
        let mut values = Vec::new();
        while let Some(value) = seq.next_element_seed(Node {
            budget: self.budget,
            depth: self.depth + 1,
        })? {
            values.push(value);
        }
        Ok(Value::Array(values))
    }
    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> std::result::Result<Value, A::Error> {
        let mut values = Map::new();
        while let Some(key) = map.next_key::<String>()? {
            if values.contains_key(&key) {
                return Err(de::Error::custom("duplicate"));
            }
            let value = map.next_value_seed(Node {
                budget: self.budget,
                depth: self.depth + 1,
            })?;
            values.insert(key, value);
        }
        Ok(Value::Object(values))
    }
}

fn integer_tokens(raw: &[u8]) -> Result<()> {
    let mut bytes = raw.iter().copied().peekable();
    while let Some(byte) = bytes.next() {
        match byte {
            b'"' => {
                while let Some(byte) = bytes.next() {
                    match byte {
                        b'\\' => {
                            bytes.next();
                        }
                        b'"' => break,
                        _ => {}
                    }
                }
            }
            b'-' | b'0'..=b'9' => {
                while bytes.peek().is_some_and(u8::is_ascii_digit) {
                    bytes.next();
                }
                ensure(
                    !matches!(bytes.peek(), Some(b'.' | b'e' | b'E')),
                    MetadataError::Json,
                )?;
            }
            _ => {}
        }
    }
    Ok(())
}

pub(super) fn decode(
    raw: &[u8],
    maximum: usize,
    depth: usize,
    nodes: usize,
    booleans: bool,
    canonical: bool,
) -> Result<Value> {
    ensure(raw.len() <= maximum, MetadataError::Bounds)?;
    std::str::from_utf8(raw).map_err(|_| MetadataError::Json)?;
    // Serde maps integer -0 to f64, so reject float spellings before that distinction is lost.
    integer_tokens(raw)?;
    let mut decoder = serde_json::Deserializer::from_slice(raw);
    let mut budget = DecodeBudget {
        remaining: nodes,
        depth,
        booleans,
    };
    let value = Node {
        budget: &mut budget,
        depth: 0,
    }
    .deserialize(&mut decoder)
    .map_err(|_| MetadataError::Json)?;
    decoder.end().map_err(|_| MetadataError::Json)?;
    if canonical {
        ensure(encode(&value)? == raw, MetadataError::Canonical)?;
    }
    Ok(value)
}

fn encode(value: &Value) -> Result<Vec<u8>> {
    fn write(value: &Value, output: &mut Vec<u8>) -> Result<()> {
        match value {
            Value::Object(fields) => {
                output.push(b'{');
                let sorted: BTreeMap<_, _> = fields.iter().collect();
                for (index, (key, value)) in sorted.into_iter().enumerate() {
                    if index != 0 {
                        output.push(b',');
                    }
                    serde_json::to_writer(&mut *output, key).map_err(|_| MetadataError::Json)?;
                    output.push(b':');
                    write(value, output)?;
                }
                output.push(b'}');
            }
            Value::Array(values) => {
                output.push(b'[');
                for (index, value) in values.iter().enumerate() {
                    if index != 0 {
                        output.push(b',');
                    }
                    write(value, output)?;
                }
                output.push(b']');
            }
            _ => serde_json::to_writer(output, value).map_err(|_| MetadataError::Json)?,
        }
        Ok(())
    }
    let mut output = Vec::new();
    write(value, &mut output)?;
    Ok(output)
}
fn hash(raw: &[u8]) -> String {
    format!("{:x}", Sha256::digest(raw))
}
fn value_hash(value: &Value) -> Result<String> {
    Ok(hash(&encode(value)?))
}
fn ensure(condition: bool, error: MetadataError) -> Result<()> {
    if condition { Ok(()) } else { Err(error) }
}
fn object(value: &Value) -> Result<&Map<String, Value>> {
    value.as_object().ok_or(MetadataError::Shape)
}
fn array(value: &Value) -> Result<&Vec<Value>> {
    value.as_array().ok_or(MetadataError::Shape)
}
fn string(value: &Value) -> Result<&str> {
    value.as_str().ok_or(MetadataError::Shape)
}
fn text(value: &Value, max: usize) -> Result<&str> {
    let s = string(value)?;
    ensure(!s.is_empty() && s.len() <= max, MetadataError::Bounds)?;
    Ok(s)
}
fn integer(value: &Value, min: i64, max: i64) -> Result<i64> {
    let n = value.as_i64().ok_or(MetadataError::Shape)?;
    ensure((min..=max).contains(&n), MetadataError::Bounds)?;
    Ok(n)
}
fn hex(value: &Value, length: usize) -> Result<&str> {
    let s = string(value)?;
    ensure(
        s.len() == length
            && s.bytes()
                .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b)),
        MetadataError::Identity,
    )?;
    Ok(s)
}
fn sri(value: &Value) -> Result<()> {
    let encoded = string(value)?
        .strip_prefix("sha256-")
        .ok_or(MetadataError::Identity)?;
    base64_bytes(encoded, 32)
}
fn base64_bytes(encoded: &str, size: usize) -> Result<()> {
    let raw = STANDARD
        .decode(encoded)
        .map_err(|_| MetadataError::Identity)?;
    ensure(
        raw.len() == size && STANDARD.encode(&raw) == encoded,
        MetadataError::Identity,
    )
}
fn version(value: &Value) -> Result<&str> {
    let s = string(value)?;
    let mut pieces = s.split('.');
    ensure(pieces.next() == Some("0"), MetadataError::Shape)?;
    for _ in 0..2 {
        let part = pieces.next().ok_or(MetadataError::Shape)?;
        ensure(
            !part.is_empty()
                && part.bytes().all(|b| b.is_ascii_digit())
                && (part == "0" || !part.starts_with('0')),
            MetadataError::Shape,
        )?;
    }
    ensure(pieces.next().is_none(), MetadataError::Shape)?;
    Ok(s)
}

struct Contract {
    projection: Value,
}
impl Contract {
    fn load() -> Result<Self> {
        Ok(Self {
            projection: serde_json::from_slice(REGISTRY).map_err(|_| MetadataError::Json)?,
        })
    }
    fn registry(&self) -> &Value {
        &self.projection["registry"]
    }
    fn limit(&self, name: &str) -> Result<usize> {
        usize::try_from(
            self.registry()["limits"][name]
                .as_u64()
                .ok_or(MetadataError::Shape)?,
        )
        .map_err(|_| MetadataError::Bounds)
    }
    fn exact(&self, value: &Value, name: &str) -> Result<()> {
        keys(
            value,
            array(&self.registry()["wire_fields"][name])?
                .iter()
                .map(string)
                .collect::<Result<Vec<_>>>()?,
        )
    }
    fn decode(&self, raw: &[u8], limit: &str, booleans: bool, canonical: bool) -> Result<Value> {
        decode(
            raw,
            self.limit(limit)?,
            self.limit("max_core_depth")?,
            self.limit("max_json_nodes")?,
            booleans,
            canonical,
        )
    }
    fn path(&self, value: &Value) -> Result<OutputPath> {
        let path = text(value, self.limit("max_nix_store_path_bytes")?)?;
        let rest = path
            .strip_prefix("/nix/store/")
            .ok_or(MetadataError::Shape)?;
        let (digest, name) = rest.split_once('-').ok_or(MetadataError::Shape)?;
        ensure(
            digest.len() == 32
                && digest
                    .bytes()
                    .all(|b| b"0123456789abcdfghijklmnpqrsvwxyz".contains(&b)),
            MetadataError::Shape,
        )?;
        ensure(
            !name.is_empty()
                && name.len() <= 211
                && name
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b"+._?=-".contains(&b))
                && !name.ends_with(".drv")
                && name != "."
                && name != "..",
            MetadataError::Shape,
        )?;
        Ok(OutputPath(path.into()))
    }
    fn file(&self, value: &Value, maximum: &str) -> Result<FileIdentity> {
        self.exact(value, "file_identity")?;
        let name = text(&value["name"], self.limit("max_name_bytes")?)?;
        ensure(
            name.len() <= 512
                && name.as_bytes()[0].is_ascii_alphanumeric()
                && name
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b"._+-".contains(&b))
                && !name.ends_with(".drv"),
            MetadataError::File,
        )?;
        let sha = hex(&value["sha256"], 64)?;
        let size = integer(
            &value["size"],
            1,
            self.limit("max_native_file_bytes")? as i64,
        )? as usize;
        ensure(size <= self.limit(maximum)?, MetadataError::Bounds)?;
        Ok(FileIdentity {
            name: name.into(),
            sha: sha.into(),
            size,
        })
    }
}

fn keys<'a>(value: &Value, expected: impl IntoIterator<Item = &'a str>) -> Result<()> {
    let expected: BTreeSet<_> = expected.into_iter().collect();
    let actual: BTreeSet<_> = object(value)?.keys().map(String::as_str).collect();
    ensure(actual == expected, MetadataError::Shape)
}
fn keys_like(value: &Value, expected: &Value) -> Result<()> {
    keys(value, object(expected)?.keys().map(String::as_str))
}
fn keys_array(value: &Value, expected: &Value) -> Result<()> {
    keys(
        value,
        array(expected)?
            .iter()
            .map(string)
            .collect::<Result<Vec<_>>>()?,
    )
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FileIdentity {
    pub(crate) name: String,
    pub(crate) sha: String,
    pub(crate) size: usize,
}
impl FileIdentity {
    pub(crate) fn verify(&self, raw: &[u8]) -> Result<()> {
        ensure(
            raw.len() == self.size && hash(raw) == self.sha,
            MetadataError::File,
        )
    }
}

struct Core {
    value: Value,
    files: BTreeMap<String, FileIdentity>,
    metadata: BTreeSet<String>,
    roots: BTreeMap<String, BTreeMap<String, OutputPath>>,
}

fn context(contract: &Contract, core: &Value) -> Result<()> {
    let ctx = &core["context"];
    let engineering = ctx["kind"] == "engineering";
    ensure(
        engineering || ctx["kind"] == "release",
        MetadataError::Shape,
    )?;
    contract.exact(
        ctx,
        if engineering {
            "engineering_context"
        } else {
            "release_context"
        },
    )?;
    for key in ["build_revision", "change_source"] {
        hex(&ctx[key], 40)?;
    }
    for key in ["build_tree_sha256", "source_tree_sha256"] {
        hex(&ctx[key], 64)?;
    }
    ensure(
        (ctx["build_revision"] == ctx["change_source"]) == engineering,
        MetadataError::Identity,
    )?;
    if engineering {
        let seed = &ctx["version_seed"];
        match string(&seed["kind"])? {
            "committed" => {
                contract.exact(seed, "committed_version_seed")?;
                ensure(
                    ctx["build_tree_sha256"] == ctx["source_tree_sha256"],
                    MetadataError::Identity,
                )?;
            }
            "engineering-version" => {
                contract.exact(seed, "engineering_version_seed")?;
                text(&seed["version"], 32)?;
                version(&seed["version"])?;
                ensure(
                    seed["version"] == core["version"]
                        && seed["build_tree_sha256"] == ctx["build_tree_sha256"],
                    MetadataError::Identity,
                )?;
                for key in [
                    "recipe_sha256",
                    "generated_diff_sha256",
                    "build_tree_sha256",
                ] {
                    hex(&seed[key], 64)?;
                }
            }
            _ => return Err(MetadataError::Shape),
        }
    } else {
        hex(&ctx["plan_id"], 64)?;
        hex(&ctx["seed_receipt_sha256"], 64)?;
    }
    Ok(())
}

fn core(contract: &Contract, raw: &[u8]) -> Result<Core> {
    let value = contract.decode(raw, "max_core_bytes", false, true)?;
    contract.exact(&value, "core")?;
    let reg = contract.registry();
    ensure(
        value["schema_version"] == 2
            && value["policy"] == reg["policy"]
            && value["contract_registry_sha256"] == hash(REGISTRY),
        MetadataError::Identity,
    )?;
    version(&value["version"])?;
    context(contract, &value)?;
    let inputs = &value["build_inputs"];
    contract.exact(inputs, "build_inputs")?;
    for key in [
        "flake_lock_sha256",
        "cargo_lock_sha256",
        "recipe_set_sha256",
    ] {
        hex(&inputs[key], 64)?;
    }
    let starter = &inputs["starter"];
    contract.exact(starter, "starter_inputs")?;
    hex(&starter["framework_revision"], 40)?;
    sri(&starter["framework_nar_hash"])?;
    for key in ["flake_sha256", "lock_sha256", "cabal_project_sha256"] {
        hex(&starter[key], 64)?;
    }
    keys_array(
        &value["native_neo"],
        &contract.projection["resolved"]["native_neo"]["targets"],
    )?;
    keys_array(
        &value["native_installers"],
        &reg["roles"]["native_installers"]["platforms"],
    )?;
    keys_like(&value["bootstraps"], &reg["roles"]["bootstraps"])?;
    keys_array(
        &value["auxiliary_assets"],
        &contract.projection["resolved"]["auxiliary_assets"],
    )?;
    keys_array(&value["packages"], &reg["roles"]["nix_packages"]["systems"])?;
    let mut roots = BTreeMap::new();
    let mut distinct = BTreeSet::new();
    for (system, outputs) in object(&value["packages"])? {
        keys_array(outputs, &reg["roles"]["nix_packages"]["outputs"])?;
        let mut system_roots = BTreeMap::new();
        for (role, output) in object(outputs)? {
            contract.exact(
                output,
                if role == "pair" {
                    "pair_output"
                } else {
                    "package_output"
                },
            )?;
            let path = contract.path(&output["store_path"])?;
            ensure(distinct.insert(path.clone()), MetadataError::Graph)?;
            sri(&output["nar_hash"])?;
            integer(&output["nar_size"], 1, i64::MAX)?;
            for key in ["closure_id", "build_receipt_sha256"] {
                hex(&output[key], 64)?;
            }
            if role == "pair" {
                contract.exact(&output["members"], "pair_members")?;
                for (member, path) in object(&output["members"])? {
                    ensure(path == &outputs[member]["store_path"], MetadataError::Graph)?;
                }
            }
            system_roots.insert(role.clone(), path);
        }
        roots.insert(system.clone(), system_roots);
    }
    ensure(
        distinct.len() as u64
            == reg["roles"]["nix_packages"]["output_receipt_count"]
                .as_u64()
                .ok_or(MetadataError::Shape)?,
        MetadataError::Graph,
    )?;
    let mut files = BTreeMap::new();
    let mut metadata = BTreeSet::new();
    let mut add = |identity: FileIdentity, collect: bool| -> Result<()> {
        ensure(
            identity.name != "candidate.json" && !files.contains_key(&identity.name),
            MetadataError::File,
        )?;
        if collect {
            metadata.insert(identity.name.clone());
        }
        files.insert(identity.name.clone(), identity);
        Ok(())
    };
    for (target, entry) in object(&value["native_neo"])? {
        contract.exact(entry, "native_neo_entry")?;
        let file = contract.file(&entry["file"], "max_native_file_bytes")?;
        let required = array(&contract.projection["resolved"]["required_assets"])?;
        ensure(
            required
                .iter()
                .filter_map(Value::as_str)
                .any(|name| name == file.name && name.ends_with(target)),
            MetadataError::File,
        )?;
        hex(&entry["build_receipt_sha256"], 64)?;
        add(file, false)?;
    }
    for (target, entry) in object(&value["native_installers"])? {
        contract.exact(entry, "native_installer_entry")?;
        let file = contract.file(&entry["file"], "max_native_file_bytes")?;
        ensure(
            file.name
                == format!(
                    "{}{}",
                    string(&reg["roles"]["native_installers"]["filename_prefix"])?,
                    target
                ),
            MetadataError::File,
        )?;
        hex(&entry["build_receipt_sha256"], 64)?;
        add(file, false)?;
    }
    for (role, entry) in object(&value["bootstraps"])? {
        contract.exact(entry, "bootstrap_entry")?;
        let file = contract.file(&entry["file"], "max_bootstrap_file_bytes")?;
        ensure(
            file.name == reg["roles"]["bootstraps"][role]["filename"]
                && entry["locator_input"] == reg["roles"]["bootstraps"][role]["locator_input"],
            MetadataError::Identity,
        )?;
        for key in ["english_source_sha256", "generator_recipe_sha256"] {
            hex(&entry[key], 64)?;
        }
        add(file, true)?;
    }
    for (name, entry) in object(&value["auxiliary_assets"])? {
        let file = contract.file(entry, "max_metadata_file_bytes")?;
        ensure(file.name == *name, MetadataError::File)?;
        add(file, true)?;
    }
    for (role, name) in object(&reg["metadata_files"])? {
        let file = contract.file(&value[role], "max_table_bytes")?;
        ensure(file.name == *name, MetadataError::File)?;
        add(file, true)?;
    }
    Ok(Core {
        value,
        files,
        metadata,
        roots,
    })
}

fn sorted_strings(value: &Value, maximum: usize) -> Result<Vec<&str>> {
    let list = array(value)?;
    ensure(list.len() <= maximum, MetadataError::Bounds)?;
    let strings = list.iter().map(string).collect::<Result<Vec<_>>>()?;
    ensure(
        strings.windows(2).all(|pair| pair[0] < pair[1]),
        MetadataError::Shape,
    )?;
    Ok(strings)
}

fn closures(
    contract: &Contract,
    core: &Core,
    table: &Value,
) -> Result<BTreeMap<String, SelectedOutputs>> {
    contract.exact(table, "closure_table")?;
    ensure(table["schema_version"] == 1, MetadataError::Shape)?;
    let expected_roots: BTreeMap<_, BTreeMap<_, _>> = core
        .roots
        .iter()
        .map(|(system, roots)| {
            (
                system,
                roots
                    .iter()
                    .map(|(role, path)| (role, path.as_str()))
                    .collect(),
            )
        })
        .collect();
    ensure(
        table["roots"] == json!(expected_roots),
        MetadataError::Graph,
    )?;
    let rows = array(&table["paths"])?;
    ensure(
        !rows.is_empty() && rows.len() <= contract.limit("max_closure_paths")?,
        MetadataError::Bounds,
    )?;
    let mut trusted = BTreeMap::new();
    for key in array(&contract.registry()["trusted_cache_keys"])? {
        trusted.insert(
            string(&key["cache"])?,
            string(&key["key"])?
                .split_once(':')
                .ok_or(MetadataError::Identity)?
                .0,
        );
    }
    let mut paths = BTreeMap::<OutputPath, &Value>::new();
    let mut total_edges = 0;
    let mut previous = None;
    for row in rows {
        contract.exact(row, "closure_path")?;
        let path = contract.path(&row["store_path"])?;
        ensure(
            previous.as_ref().is_none_or(|last| last < &path),
            MetadataError::Graph,
        )?;
        previous = Some(path.clone());
        sri(&row["nar_hash"])?;
        integer(&row["nar_size"], 1, i64::MAX)?;
        let refs = sorted_strings(
            &row["references"],
            contract.limit("max_references_per_path")?,
        )?;
        for reference in &refs {
            contract.path(&Value::from(*reference))?;
        }
        total_edges += refs.len();
        ensure(
            total_edges <= contract.limit("max_closure_edges")?,
            MetadataError::Bounds,
        )?;
        let locations = array(&row["locations"])?;
        ensure(
            !locations.is_empty()
                && locations.len() <= contract.limit("max_cache_locations_per_path")?,
            MetadataError::Bounds,
        )?;
        let mut last_cache = None;
        for location in locations {
            contract.exact(location, "cache_location")?;
            let cache = text(&location["cache"], 256)?;
            let key = trusted.get(cache).ok_or(MetadataError::Identity)?;
            ensure(
                last_cache.is_none_or(|last| last < cache),
                MetadataError::Shape,
            )?;
            last_cache = Some(cache);
            let signatures = sorted_strings(
                &location["signatures"],
                contract.limit("max_signatures_per_location")?,
            )?;
            ensure(!signatures.is_empty(), MetadataError::Shape)?;
            for signature in signatures {
                ensure(signature.len() <= 256, MetadataError::Bounds)?;
                let (name, encoded) = signature.split_once(':').ok_or(MetadataError::Identity)?;
                ensure(name == *key, MetadataError::Identity)?;
                base64_bytes(encoded, 64)?;
            }
        }
        paths.insert(path, row);
    }
    for row in paths.values() {
        for reference in array(&row["references"])? {
            ensure(
                paths.contains_key(&contract.path(reference)?),
                MetadataError::Graph,
            )?;
        }
    }
    let mut union = BTreeSet::new();
    let mut reachable = BTreeMap::new();
    for (system, roots) in &core.roots {
        for (role, root) in roots {
            let row = paths.get(root).ok_or(MetadataError::Graph)?;
            let mut visited = BTreeSet::new();
            let mut pending = vec![root.clone()];
            while let Some(path) = pending.pop() {
                if !visited.insert(path.clone()) {
                    continue;
                }
                for reference in
                    array(&paths.get(&path).ok_or(MetadataError::Graph)?["references"])?
                {
                    pending.push(contract.path(reference)?);
                }
            }
            let identities = visited.iter().map(|path| {
                let row = paths[path];
                json!({"store_path": row["store_path"], "nar_hash": row["nar_hash"], "nar_size": row["nar_size"], "references": row["references"]})
            }).collect::<Vec<_>>();
            let output = &core.value["packages"][system][role];
            ensure(
                value_hash(&Value::Array(identities))? == output["closure_id"],
                MetadataError::Graph,
            )?;
            ensure(
                array(&row["locations"])?
                    .iter()
                    .any(|location| location["cache"] == "https://neohaskell.cachix.org"),
                MetadataError::Graph,
            )?;
            ensure(
                row["nar_hash"] == output["nar_hash"] && row["nar_size"] == output["nar_size"],
                MetadataError::Graph,
            )?;
            union.extend(visited.iter().cloned());
            reachable.insert(root.clone(), visited);
        }
    }
    ensure(
        union == paths.keys().cloned().collect(),
        MetadataError::Graph,
    )?;
    let mut systems = BTreeMap::new();
    for (system, roots) in &core.roots {
        let root_set: BTreeSet<_> = roots.values().cloned().collect();
        let pair = roots.get("pair").ok_or(MetadataError::Graph)?;
        let members: BTreeSet<_> = object(&core.value["packages"][system]["pair"]["members"])?
            .values()
            .map(|value| contract.path(value))
            .collect::<Result<_>>()?;
        for member in &members {
            ensure(
                reachable[member]
                    .intersection(&root_set)
                    .cloned()
                    .collect::<BTreeSet<_>>()
                    == BTreeSet::from([member.clone()]),
                MetadataError::Graph,
            )?;
        }
        let direct: BTreeSet<_> = array(&paths[pair]["references"])?
            .iter()
            .map(|value| contract.path(value))
            .collect::<Result<_>>()?;
        ensure(
            direct
                .intersection(&root_set)
                .cloned()
                .collect::<BTreeSet<_>>()
                == members,
            MetadataError::Graph,
        )?;
        let mut paired = members;
        paired.insert(pair.clone());
        ensure(
            reachable[pair]
                .intersection(&root_set)
                .cloned()
                .collect::<BTreeSet<_>>()
                == paired,
            MetadataError::Graph,
        )?;
        let mut selected = BTreeMap::new();
        for path in &reachable[pair] {
            let row = paths[path];
            selected.insert(
                path.clone(),
                NarMetadata {
                    hash: string(&row["nar_hash"])?.into(),
                    size: integer(&row["nar_size"], 1, i64::MAX)? as u64,
                    locations: array(&row["locations"])?
                        .iter()
                        .map(|location| cache_witness(contract, location))
                        .collect::<Result<_>>()?,
                    references: array(&row["references"])?
                        .iter()
                        .map(|value| contract.path(value))
                        .collect::<Result<_>>()?,
                },
            );
        }
        systems.insert(
            system.clone(),
            SelectedOutputs {
                outputs: roots.clone(),
                closure: selected,
            },
        );
    }
    Ok(systems)
}

fn receipts(contract: &Contract, core: &Value, table: &Value) -> Result<()> {
    contract.exact(table, "build_receipts_table")?;
    ensure(table["schema_version"] == 1, MetadataError::Shape)?;
    let mut expected = BTreeMap::new();
    for (field, role, command) in [
        ("native_neo", "native-neo", "neo"),
        ("native_installers", "native-installer", "neo-installer"),
    ] {
        for (platform, entry) in object(&core[field])? {
            expected.insert(
                (role.to_owned(), platform.clone()),
                (
                    entry,
                    json!({"sha256": entry["file"]["sha256"], "size": entry["file"]["size"]}),
                    json!({command: core["version"]}),
                    "native_output_identity",
                ),
            );
        }
    }
    for (system, outputs) in object(&core["packages"])? {
        for (role, entry) in object(outputs)? {
            let pair = role == "pair";
            let mut identity = json!({"store_path": entry["store_path"], "nar_hash": entry["nar_hash"], "nar_size": entry["nar_size"]});
            let mut versions = Map::new();
            if pair {
                identity["members"] = entry["members"].clone();
                for member in object(&entry["members"])?.keys() {
                    versions.insert(member.clone(), core["version"].clone());
                }
            } else {
                versions.insert(role.clone(), core["version"].clone());
            }
            expected.insert(
                (format!("nix-{role}"), system.clone()),
                (
                    entry,
                    identity,
                    Value::Object(versions),
                    if pair {
                        "pair_output_identity"
                    } else {
                        "nix_output_identity"
                    },
                ),
            );
        }
    }
    let rows = array(&table["receipts"])?;
    ensure(
        rows.len() == expected.len()
            && rows.len() as u64
                == contract.registry()["roles"]["output_receipt_count"]
                    .as_u64()
                    .ok_or(MetadataError::Shape)?,
        MetadataError::Receipt,
    )?;
    let context_digest = value_hash(&core["context"])?;
    let inputs_digest = value_hash(&core["build_inputs"])?;
    let mut previous = None;
    for row in rows {
        contract.exact(row, "build_receipt")?;
        let key = (
            text(&row["role"], 64)?.to_owned(),
            text(&row["platform"], 128)?.to_owned(),
        );
        ensure(
            previous.as_ref().is_none_or(|last| last < &key),
            MetadataError::Receipt,
        )?;
        previous = Some(key.clone());
        let (entry, identity, versions, fields) =
            expected.get(&key).ok_or(MetadataError::Receipt)?;
        let bindings = [
            ("context_sha256", context_digest.as_str()),
            (
                "source_tree_sha256",
                string(&core["context"]["source_tree_sha256"])?,
            ),
            (
                "build_tree_sha256",
                string(&core["context"]["build_tree_sha256"])?,
            ),
            ("build_inputs_sha256", inputs_digest.as_str()),
            (
                "recipe_set_sha256",
                string(&core["build_inputs"]["recipe_set_sha256"])?,
            ),
            (
                "producer_sha256",
                string(&core["build_inputs"]["recipe_set_sha256"])?,
            ),
        ];
        for (field, value) in bindings {
            hex(&row[field], 64)?;
            ensure(row[field] == value, MetadataError::Receipt)?;
        }
        ensure(row["version"] == core["version"], MetadataError::Receipt)?;
        text(&row["builder_system"], 128)?;
        contract.exact(&row["output_identity"], fields)?;
        ensure(&row["output_identity"] == identity, MetadataError::Receipt)?;
        let observations = &row["verification_observations"];
        contract.exact(observations, "verification_observations")?;
        for field in ["argv_sha256", "result_sha256", "toolchain_sha256"] {
            hex(&observations[field], 64)?;
        }
        ensure(
            &observations["reported_versions"] == versions
                && entry["build_receipt_sha256"] == value_hash(row)?,
            MetadataError::Receipt,
        )?;
    }
    Ok(())
}

fn starter(contract: &Contract, core: &Value, files: &BTreeMap<&str, &[u8]>) -> Result<()> {
    let input = &core["build_inputs"]["starter"];
    let assets = &core["auxiliary_assets"];
    let names = &contract.projection["resolved"]["auxiliary_assets"];
    let lookup = |suffix: &str, hash_key: &str| -> Result<&[u8]> {
        let matches: Vec<_> = array(names)?
            .iter()
            .filter_map(Value::as_str)
            .filter(|name| name.ends_with(suffix))
            .collect();
        ensure(matches.len() == 1, MetadataError::Starter)?;
        let name = matches[0];
        let bytes = files.get(name).copied().ok_or(MetadataError::File)?;
        ensure(
            hash(bytes) == input[hash_key] && assets[name]["sha256"] == input[hash_key],
            MetadataError::Starter,
        )?;
        Ok(bytes)
    };
    let flake = std::str::from_utf8(lookup("-flake.nix", "flake_sha256")?)
        .map_err(|_| MetadataError::Starter)?;
    let project = std::str::from_utf8(lookup("-cabal.project", "cabal_project_sha256")?)
        .map_err(|_| MetadataError::Starter)?;
    let lock = contract.decode(
        lookup("-flake.lock", "lock_sha256")?,
        "max_metadata_file_bytes",
        true,
        false,
    )?;
    let revision = string(&input["framework_revision"])?;
    let captures = |pattern: &str, text: &str| -> Result<Vec<String>> {
        let pattern = regex::Regex::new(pattern).map_err(|_| MetadataError::Starter)?;
        Ok(pattern
            .captures_iter(text)
            .map(|capture| capture[1].to_owned())
            .collect())
    };
    let revisions = captures(r"\?rev=([0-9a-f]{40})", flake)?;
    let declarations = captures(
        r#"(?m)^[\s\x1c-\x1f]*neohaskellCommit[\s\x1c-\x1f]*=[\s\x1c-\x1f]*"([0-9a-f]{40})"[\s\x1c-\x1f]*;"#,
        flake,
    )?;
    let tags = captures(
        r"(?m)^[\s\x1c-\x1f]*tag:[\s\x1c-\x1f]*([0-9a-f]{40})[\s\x1c-\x1f]*$",
        project,
    )?;
    let locked = &lock["nodes"]["neohaskell"]["locked"];
    ensure(
        revisions == [revision]
            && declarations == [revision]
            && !tags.is_empty()
            && tags.iter().all(|tag| tag == revision)
            && locked["rev"] == revision
            && locked["narHash"] == input["framework_nar_hash"],
        MetadataError::Starter,
    )?;
    let expected = if core["context"]["kind"] == "engineering" {
        &contract.registry()["source_inputs"]["engineering_starter_framework_revision"]
    } else {
        &core["context"]["build_revision"]
    };
    ensure(
        &input["framework_revision"] == expected,
        MetadataError::Starter,
    )
}

pub(crate) struct MetadataManifest {
    pub(crate) candidate_id: String,
    pub(crate) version: String,
    pub(crate) metadata: Vec<FileIdentity>,
    pub(crate) native_installer: FileIdentity,
    pub(crate) required_assets: Vec<FileIdentity>,
}

pub(crate) fn release_core_limit() -> Result<usize> {
    Contract::load()?.limit("max_core_bytes")
}

#[cfg(target_os = "linux")]
pub(crate) fn native_file_limit() -> Result<usize> {
    Contract::load()?.limit("max_native_file_bytes")
}

pub(crate) fn release_metadata_limit(name: &str) -> Result<usize> {
    let contract = Contract::load()?;
    if object(&contract.registry()["roles"]["bootstraps"])?
        .values()
        .any(|entry| entry["filename"].as_str() == Some(name))
    {
        return contract.limit("max_bootstrap_file_bytes");
    }
    if array(&contract.projection["resolved"]["auxiliary_assets"])?
        .iter()
        .any(|entry| entry.as_str() == Some(name))
    {
        return contract.limit("max_metadata_file_bytes");
    }
    if object(&contract.registry()["metadata_files"])?
        .values()
        .any(|entry| entry.as_str() == Some(name))
    {
        return contract.limit("max_table_bytes");
    }
    Err(MetadataError::File)
}

pub(crate) fn snapshot_descriptor_limit() -> Result<usize> {
    let contract = Contract::load()?;
    Ok(
        array(&contract.projection["resolved"]["auxiliary_assets"])?.len()
            + object(&contract.registry()["roles"]["bootstraps"])?.len()
            + object(&contract.registry()["metadata_files"])?.len()
            + 1,
    )
}

pub(crate) fn metadata_manifest(raw: &[u8], target: &str) -> Result<MetadataManifest> {
    let contract = Contract::load()?;
    let checked = core(&contract, raw)?;
    let native = &checked.value["native_installers"][target]["file"]["name"];
    let native_installer = checked
        .files
        .get(string(native)?)
        .ok_or(MetadataError::System)?
        .clone();
    Ok(MetadataManifest {
        candidate_id: hash(raw),
        version: string(&checked.value["version"])?.into(),
        metadata: checked
            .metadata
            .iter()
            .map(|name| checked.files[name].clone())
            .collect(),
        native_installer,
        required_assets: checked.files.values().cloned().collect(),
    })
}

pub(crate) fn parse(inputs: MetadataInputs<'_>) -> Result<ParsedCandidate> {
    let contract = Contract::load()?;
    let checked = core(&contract, inputs.core)?;
    ensure(
        inputs.files.keys().copied().collect::<BTreeSet<_>>()
            == checked.metadata.iter().map(String::as_str).collect(),
        MetadataError::File,
    )?;
    for name in &checked.metadata {
        checked.files[name].verify(inputs.files.get(name.as_str()).ok_or(MetadataError::File)?)?;
    }
    starter(&contract, &checked.value, &inputs.files)?;
    let closure_name = string(&checked.value["closures"]["name"])?;
    let receipt_name = string(&checked.value["build_receipts"]["name"])?;
    let closure_table =
        contract.decode(inputs.files[closure_name], "max_table_bytes", false, true)?;
    let receipt_table =
        contract.decode(inputs.files[receipt_name], "max_table_bytes", false, true)?;
    let systems = closures(&contract, &checked, &closure_table)?;
    receipts(&contract, &checked.value, &receipt_table)?;
    let id = hash(inputs.core);
    for entry in object(&checked.value["bootstraps"])?.values() {
        let bytes = inputs.files[string(&entry["file"]["name"])?];
        ensure(
            !bytes
                .windows(id.len())
                .any(|window| window == id.as_bytes()),
            MetadataError::Identity,
        )?;
    }
    Ok(ParsedCandidate {
        id,
        version: string(&checked.value["version"])?.into(),
        systems,
    })
}

#[cfg(test)]
mod tests;
