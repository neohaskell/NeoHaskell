use super::super::{
    ConfigurationEntry, ConfigurationFile, InputIdentity, MAX_CONFIGURATION_BYTES, POLICY_LEAF,
    TransactionError, configuration_source_with, include_inputs::EffectivePolicy,
    protected_configuration_path_with,
};
use super::{PLIST, SUPERVISOR_PATH};
use serde_json::Value;
use std::ffi::{CString, c_void};
use std::fs::{self, File};
use std::io::{self, Read};
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

const STANDARD: &str = "/private/etc/nix/nix.conf";
const CUSTOM: &str = "/private/etc/nix/nix.custom.conf";
const LOGICAL_CUSTOM: &str = "/etc/nix/nix.custom.conf";
const RECEIPT: &str = "/nix/receipt.json";
const STATIC: &str = "/private/etc/static";
const SYSTEM_PROFILE: &str = "/nix/var/nix/profiles/system";
const CURRENT_SYSTEM: &str = "/run/current-system";
const CUSTOM_ENTRY: &str = "nix/nix.custom.conf";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct DiskObservation {
    pub(super) source: SourceRoute,
    pub(super) policy: DiskPolicy,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum SourceRoute {
    Unknown,
    VendorCustom,
    DarwinGenerated,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum DiskPolicy {
    Unknown,
    RequiredCacheMissing,
    Satisfies,
}

pub(super) fn observe() -> DiskObservation {
    capture().unwrap_or(DiskObservation {
        source: SourceRoute::Unknown,
        policy: DiskPolicy::Unknown,
    })
}

#[derive(PartialEq, Eq)]
struct Deployment {
    current: InputIdentity,
    generation: PathBuf,
    marker: ConfigurationFile,
    projected: PathBuf,
}

fn capture() -> Result<DiskObservation, TransactionError> {
    require_etc_alias()?;
    let standard = read_configuration(STANDARD)?;
    let custom = read_configuration(CUSTOM)?;
    if !is_direct(&standard.0) {
        return Err(TransactionError::Io);
    }
    let active = deployment()?;
    let (source, receipt, service) = if let Some(active) = &active {
        let generated_standard = active.projected.join("nix/nix.conf");
        if exists(&generated_standard)? {
            return Err(TransactionError::Io);
        }
        let generated_custom = active.projected.join(CUSTOM_ENTRY);
        if exists(&generated_custom)? {
            let expected_link = Path::new("/etc/static").join(CUSTOM_ENTRY);
            let resolved = protect(&generated_custom, true).map_err(|_| TransactionError::Io)?;
            if !is_store_path(&resolved)
                || fs::read_link(CUSTOM).ok().as_ref() != Some(&expected_link)
                || !matches!(&custom.0, ConfigurationFile::Present { resolved: actual, .. } if actual == &resolved)
            {
                return Err(TransactionError::Io);
            }
            (SourceRoute::DarwinGenerated, None, None)
        } else {
            vendor_files(&custom)?
        }
    } else {
        vendor_files(&custom)?
    };

    let leaf_path = Path::new(STANDARD).with_file_name(POLICY_LEAF);
    let leaf_path = leaf_path.to_str().ok_or(TransactionError::Io)?;
    let (policy, leaf) = disk_policy(&standard.1, &custom.1, leaf_path)?;
    if read_configuration(STANDARD)?.0 != standard.0
        || read_configuration(CUSTOM)?.0 != custom.0
        || active != deployment()?
        || receipt.is_some_and(
            |input| !matches!(read_configuration(RECEIPT), Ok((fresh, _)) if fresh == input),
        )
        || service.is_some_and(
            |input| !matches!(read_configuration(PLIST), Ok((fresh, _)) if fresh == input),
        )
        || leaf.is_some_and(
            |input| !matches!(read_configuration(leaf_path), Ok((fresh, _)) if fresh == input),
        )
    {
        return Err(TransactionError::Io);
    }
    if let Some(active) = &active {
        let projected_custom = active.projected.join(CUSTOM_ENTRY);
        if exists(&active.projected.join("nix/nix.conf"))?
            || exists(&projected_custom)? != (source == SourceRoute::DarwinGenerated)
            || (source == SourceRoute::DarwinGenerated
                && !matches!(protect(&projected_custom, true), Ok(path)
                    if matches!(&custom.0, ConfigurationFile::Present { resolved, .. } if resolved == &path)))
        {
            return Err(TransactionError::Io);
        }
    }
    require_etc_alias()?;
    Ok(DiskObservation { source, policy })
}

fn vendor_files(
    custom: &(ConfigurationFile, Vec<u8>),
) -> Result<
    (
        SourceRoute,
        Option<ConfigurationFile>,
        Option<ConfigurationFile>,
    ),
    TransactionError,
> {
    let receipt = read_configuration(RECEIPT)?;
    let service = read_configuration(PLIST)?;
    if !is_direct(&custom.0)
        || !is_direct(&receipt.0)
        || !is_direct(&service.0)
        || !vendor_receipt(&receipt.1, &custom.1)
    {
        return Err(TransactionError::Io);
    }
    Ok((SourceRoute::VendorCustom, Some(receipt.0), Some(service.0)))
}

fn is_direct(input: &ConfigurationFile) -> bool {
    matches!(input,
        ConfigurationFile::Present { entry: ConfigurationEntry::Direct, file, .. } if file.links == 1)
}

fn vendor_receipt(bytes: &[u8], custom: &[u8]) -> bool {
    let Ok(receipt) = serde_json::from_slice::<Value>(bytes) else {
        return false;
    };
    let Some(actions) = receipt["actions"].as_array() else {
        return false;
    };
    let configure = complete(actions, "configure_nix");
    let place = complete(
        std::slice::from_ref(&configure["place_nix_configuration"]),
        "place_nix_configuration",
    );
    let custom_action = complete(
        std::slice::from_ref(&place["create_or_merge_custom_nix_config"]),
        "create_or_merge_nix_config",
    );
    let initialize = complete(actions, "configure_determinate_nixd_init_service");
    let service = complete(
        std::slice::from_ref(&initialize["configure_init_service"]),
        "configure_init_service",
    );
    receipt["version"] == "3.23.1"
        && complete(actions, "provision_determinate_nixd")["binary_location"] == SUPERVISOR_PATH
        && service["service_dest"] == PLIST
        && place.get("create_or_merge_standard_nix_config") == Some(&Value::Null)
        && custom_action["path"] == LOGICAL_CUSTOM
        && custom_action["header"]
            .as_str()
            .is_some_and(|header| !header.is_empty() && custom.starts_with(header.as_bytes()))
}

fn complete<'a>(actions: &'a [Value], name: &str) -> &'a Value {
    let mut found = actions
        .iter()
        .filter(|entry| entry["action"]["action_name"] == name);
    match (found.next(), found.next()) {
        (Some(entry), None) if entry["state"] == "Completed" => &entry["action"],
        _ => &Value::Null,
    }
}

fn deployment() -> Result<Option<Deployment>, TransactionError> {
    let profile = exists(Path::new(SYSTEM_PROFILE))?;
    let projected = exists(Path::new(STATIC))?;
    let current = fs::symlink_metadata(CURRENT_SYSTEM);
    if !profile && !projected && matches!(&current, Err(e) if e.kind() == io::ErrorKind::NotFound) {
        return Ok(None);
    }
    let io = |_| TransactionError::Io;
    let current = current.map_err(io)?;
    let generation = protect(Path::new(SYSTEM_PROFILE), true).map_err(io)?;
    let run = Path::new("/run");
    let run_metadata = fs::symlink_metadata(run).map_err(io)?;
    let run_target = fs::read_link(run).map_err(io)?;
    require_no_acl(run).map_err(io)?;
    require_no_acl(Path::new(CURRENT_SYSTEM)).map_err(io)?;
    protect_configuration_path(Path::new("/private/var")).map_err(io)?;
    if !is_store_path(&generation)
        || generation.parent() != Some(Path::new("/nix/store"))
        || run_metadata.uid() != 0
        || !system_alias(&run_target, Path::new("/private/var/run"))
        || !current.file_type().is_symlink()
        || current.uid() != 0
        || fs::read_link(CURRENT_SYSTEM).map_err(io)? != generation
    {
        return Err(TransactionError::Io);
    }
    let current = InputIdentity::of(&current);
    let marker_path = generation.join("systemConfig");
    let (marker, bytes) =
        configuration_source_with(&marker_path, &|path| protect(path, true), &mut read_limited)?;
    let bytes = bytes.strip_suffix(b"\n").unwrap_or(&bytes);
    let projected = protect(Path::new(STATIC), true).map_err(io)?;
    if !is_direct(&marker)
        || bytes != generation.as_os_str().as_encoded_bytes()
        || !is_store_path(&projected)
        || protect(&generation.join("etc"), true).map_err(io)? != projected
        || current != InputIdentity::of(&fs::symlink_metadata(CURRENT_SYSTEM).map_err(io)?)
    {
        return Err(TransactionError::Io);
    }
    Ok(Some(Deployment {
        current,
        generation,
        marker,
        projected,
    }))
}

fn exists(path: &Path) -> Result<bool, TransactionError> {
    match fs::symlink_metadata(path) {
        Ok(_) => Ok(true),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(false),
        Err(_) => Err(TransactionError::Io),
    }
}

fn is_store_path(path: &Path) -> bool {
    path.strip_prefix("/nix/store").is_ok_and(|suffix| {
        suffix.components().next().is_some_and(|component| {
            let name = component.as_os_str().as_encoded_bytes();
            name.get(32) == Some(&b'-')
                && name.len() > 33
                && name[..32]
                    .iter()
                    .all(|byte| b"0123456789abcdfghijklmnpqrsvwxyz".contains(byte))
        })
    })
}

fn require_etc_alias() -> Result<(), TransactionError> {
    let path = Path::new("/etc");
    if protect_configuration_path(path).ok().as_deref() != Some(Path::new("/private/etc"))
        || !matches!(fs::read_link(path), Ok(target)
            if system_alias(&target, Path::new("/private/etc")))
    {
        return Err(TransactionError::Io);
    }
    Ok(())
}

fn system_alias(target: &Path, expected: &Path) -> bool {
    target == expected
        || expected
            .strip_prefix("/")
            .is_ok_and(|relative| target == relative)
}

pub(super) fn protect_configuration_path(path: &Path) -> io::Result<PathBuf> {
    protect(path, false)
}

fn protect(path: &Path, allow_shared_store: bool) -> io::Result<PathBuf> {
    protected_configuration_path_with(path, allow_shared_store, |component, _| {
        require_no_acl(component)
    })
}

unsafe extern "C" {
    fn acl_get_link_np(path: *const libc::c_char, acl_type: libc::c_int) -> *mut c_void;
    fn acl_free(acl: *mut c_void) -> libc::c_int;
}

fn require_no_acl(path: &Path) -> io::Result<()> {
    const ACL_TYPE_EXTENDED: libc::c_int = 0x100;
    let path = CString::new(path.as_os_str().as_encoded_bytes())?;
    let acl = unsafe { acl_get_link_np(path.as_ptr(), ACL_TYPE_EXTENDED) };
    if acl.is_null() {
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(libc::ENOENT) {
            return Ok(());
        }
        return Err(error);
    }
    unsafe { acl_free(acl) };
    Err(io::Error::other("unproved configuration ACL"))
}

fn read_limited(file: &mut File) -> Result<Vec<u8>, TransactionError> {
    let mut bytes = Vec::new();
    file.take((MAX_CONFIGURATION_BYTES + 1) as u64)
        .read_to_end(&mut bytes)
        .map_err(|_| TransactionError::Io)?;
    Ok(bytes)
}

fn read_configuration(path: &str) -> Result<(ConfigurationFile, Vec<u8>), TransactionError> {
    configuration_source_with(
        Path::new(path),
        &|path| protect(path, true),
        &mut read_limited,
    )
}

fn disk_policy(
    standard: &[u8],
    custom: &[u8],
    leaf_path: &str,
) -> Result<(DiskPolicy, Option<ConfigurationFile>), TransactionError> {
    let mut effective = EffectivePolicy::default();
    let mut leaf = None;
    let saw_custom = fold_settings(standard, &mut effective, |reference, effective| {
        if reference != "!include nix.custom.conf" {
            return Err(TransactionError::Io);
        }
        fold_settings(custom, effective, |reference, effective| {
            if reference != super::super::POLICY_REFERENCE {
                return Err(TransactionError::Io);
            }
            let (input, bytes) = read_configuration(leaf_path)?;
            if !is_direct(&input) {
                return Err(TransactionError::Io);
            }
            leaf = Some(input);
            fold_settings(&bytes, effective, |_, _| Err(TransactionError::Io))?;
            Ok(())
        })?;
        Ok(())
    })?;
    if !saw_custom {
        return Err(TransactionError::Io);
    }
    let policy = match effective.required_cache_policy() {
        Ok(true) => DiskPolicy::Satisfies,
        Ok(false) => DiskPolicy::RequiredCacheMissing,
        Err(_) => DiskPolicy::Unknown,
    };
    Ok((policy, leaf))
}

fn fold_settings(
    bytes: &[u8],
    effective: &mut EffectivePolicy,
    mut include: impl FnMut(&str, &mut EffectivePolicy) -> Result<(), TransactionError>,
) -> Result<bool, TransactionError> {
    if bytes
        .iter()
        .any(|byte| byte.is_ascii_control() && !b"\t\r\n".contains(byte))
    {
        return Err(TransactionError::Io);
    }
    let mut included = false;
    for line in std::str::from_utf8(bytes)
        .map_err(|_| TransactionError::Io)?
        .lines()
    {
        let line = line
            .split('#')
            .next()
            .unwrap_or_default()
            .trim_matches([' ', '\t', '\r']);
        if line.is_empty() {
            continue;
        }
        let mut words = line.split_ascii_whitespace();
        let name = words.next().ok_or(TransactionError::Io)?;
        if matches!(name, "include" | "!include") {
            if included {
                return Err(TransactionError::Io);
            }
            include(line, effective)?;
            included = true;
        } else {
            let setting = name.strip_prefix("extra-").unwrap_or(name);
            if words.next() != Some("=")
                || !name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'-')
                || setting == "plugin-files"
                || (setting == "require-sigs" && name != setting)
                || (setting == "store" && name != setting)
            {
                return Err(TransactionError::Io);
            }
            effective.assign(name, &words.collect::<Vec<_>>())?;
        }
    }
    Ok(included)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn complete_cache_settings() -> String {
        let (urls, keys) = crate::candidate::nix_cache_settings().unwrap();
        format!("substituters = {urls}\ntrusted-public-keys = {keys}\n")
    }

    fn policy(standard: &[u8], custom: &[u8]) -> DiskPolicy {
        disk_policy(standard, custom, "/nonexistent/darwin-leaf-test.conf")
            .unwrap()
            .0
    }

    #[test]
    fn folds_custom_values_at_the_include_with_no_legacy_feature_requirement() {
        let custom = complete_cache_settings();
        assert_eq!(
            policy(b"!include nix.custom.conf\n", custom.as_bytes()),
            DiskPolicy::Satisfies
        );
        assert_eq!(
            policy(
                b"!include nix.custom.conf\nsubstituters =\n",
                custom.as_bytes()
            ),
            DiskPolicy::RequiredCacheMissing
        );
        assert_eq!(
            policy(
                b"!include nix.custom.conf\nrequire-sigs = false\n",
                custom.as_bytes()
            ),
            DiskPolicy::Unknown
        );
    }

    #[test]
    fn rejects_unbounded_or_ambiguous_configuration_topology() {
        for standard in [
            &b"!include nix.custom.conf\ninclude unrelated.conf\n"[..],
            &b"!include nix.custom.conf\nplugin-files = overlay\n"[..],
            &b"!include nix.custom.conf\nextra-plugin-files = overlay\n"[..],
            &b"!include ./nix.custom.conf\n"[..],
            &b"require-sigs = true\n"[..],
        ] {
            assert!(disk_policy(standard, b"", "/nonexistent/darwin-leaf-test.conf").is_err());
        }
    }

    #[test]
    fn required_key_collision_is_not_the_same_as_missing_policy() {
        let (_, keys) = crate::candidate::nix_cache_settings().unwrap();
        let key = keys.split_ascii_whitespace().next().unwrap();
        let (name, _) = key.split_once(':').unwrap();
        let custom = format!(
            "{}\ntrusted-public-keys = {name}:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA= {keys}\n",
            complete_cache_settings()
        );
        assert_eq!(
            policy(b"!include nix.custom.conf\n", custom.as_bytes()),
            DiskPolicy::Unknown
        );
    }

    #[test]
    fn only_recognizes_the_known_run_alias_targets() {
        let expected = Path::new("/private/var/run");
        assert!(system_alias(Path::new("private/var/run"), expected));
        assert!(system_alias(Path::new("/private/var/run"), expected));
        assert!(!system_alias(Path::new("private/var/db/run"), expected));
        assert!(!system_alias(Path::new("/tmp/run"), expected));
    }
}
