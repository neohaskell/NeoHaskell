use crate::candidate::{MetadataError, OutputPath};
use std::collections::{BTreeMap, BTreeSet};

type Result<T> = std::result::Result<T, MetadataError>;

#[derive(Debug, PartialEq, Eq)]
pub(super) struct ObservedNar {
    pub(super) hash: String,
    pub(super) size: u64,
    pub(super) references: BTreeSet<OutputPath>,
}

pub(super) fn path_info(
    bytes: &[u8],
    requested: &[OutputPath],
) -> Result<BTreeMap<OutputPath, ObservedNar>> {
    if requested.is_empty() || requested.len() > 64 {
        return Err(MetadataError::Bounds);
    }
    let value = crate::candidate::decode(bytes, 16 * 1024 * 1024, 8, 100_000, true, false)?;
    let rows = value.as_object().ok_or(MetadataError::Shape)?;
    let names: BTreeSet<_> = requested.iter().map(OutputPath::as_str).collect();
    if names.len() != requested.len() || names != rows.keys().map(String::as_str).collect() {
        return Err(MetadataError::Identity);
    }
    let mut result = BTreeMap::new();
    for path in requested {
        let row = rows[path.as_str()]
            .as_object()
            .ok_or(MetadataError::Shape)?;
        let hash = row
            .get("narHash")
            .and_then(|value| value.as_str())
            .ok_or(MetadataError::Shape)?;
        let size = row
            .get("narSize")
            .and_then(|value| value.as_u64())
            .filter(|size| *size > 0)
            .ok_or(MetadataError::Shape)?;
        let references = row
            .get("references")
            .and_then(|value| value.as_array())
            .ok_or(MetadataError::Shape)?;
        if references.len() > 512 {
            return Err(MetadataError::Bounds);
        }
        let observed = references
            .iter()
            .map(|reference| OutputPath::parse(reference.as_str().ok_or(MetadataError::Shape)?))
            .collect::<Result<BTreeSet<_>>>()?;
        if observed.len() != references.len() {
            return Err(MetadataError::Identity);
        }
        result.insert(
            path.clone(),
            ObservedNar {
                hash: hash.into(),
                size,
                references: observed,
            },
        );
    }
    Ok(result)
}

pub(super) fn converted_hashes<'a>(
    bytes: &[u8],
    hashes: &[&'a str],
) -> Result<BTreeMap<&'a str, String>> {
    if hashes.is_empty() || hashes.len() > 64 || bytes.len() > hashes.len() * 53 {
        return Err(MetadataError::Bounds);
    }
    let text = std::str::from_utf8(bytes).map_err(|_| MetadataError::Shape)?;
    if !text.ends_with('\n') {
        return Err(MetadataError::Shape);
    }
    let lines = text.lines().collect::<Vec<_>>();
    if lines.len() != hashes.len() {
        return Err(MetadataError::Identity);
    }
    let mut converted = BTreeMap::new();
    for (hash, line) in hashes.iter().zip(lines) {
        if line.len() != 52
            || !line
                .bytes()
                .all(|b| b"0123456789abcdfghijklmnpqrsvwxyz".contains(&b))
        {
            return Err(MetadataError::Shape);
        }
        if let Some(previous) = converted.insert(*hash, line.to_owned()) {
            if previous != line {
                return Err(MetadataError::Identity);
            }
        }
    }
    Ok(converted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metadata_accepts_mutable_signatures_but_rejects_missing_extra_and_duplicate_rows() {
        let path = OutputPath::parse("/nix/store/00000000000000000000000000000000-pair").unwrap();
        let row = serde_json::json!({"narHash": "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=", "narSize": 1, "references": [], "signatures": ["extra:opaque"]});
        let good = serde_json::json!({path.as_str(): row.clone()});
        let expected = std::slice::from_ref(&path);
        assert_eq!(
            path_info(&serde_json::to_vec(&good).unwrap(), expected).unwrap()[&path].size,
            1
        );
        for bad in [
            serde_json::json!({}),
            serde_json::json!({path.as_str(): null}),
            serde_json::json!({path.as_str(): row.clone(), "extra": row.clone()}),
        ] {
            assert!(path_info(&serde_json::to_vec(&bad).unwrap(), expected).is_err());
        }
        let duplicate = format!("{{{0:?}:{1},{0:?}:{1}}}", path.as_str(), row);
        assert!(path_info(duplicate.as_bytes(), expected).is_err());
    }

    #[test]
    fn maximum_reference_batch_fits_the_semantic_budget() {
        let paths: Vec<_> = (0..64)
            .map(|n| {
                OutputPath::parse(&format!(
                    "/nix/store/00000000000000000000000000000000-row-{n}"
                ))
                .unwrap()
            })
            .collect();
        let references: Vec<_> = (0..512)
            .map(|n| {
                format!(
                    "/nix/store/00000000000000000000000000000000-{}-{n:03}",
                    "r".repeat(207)
                )
            })
            .collect();
        let rows: serde_json::Map<_, _> = paths.iter().map(|path| (path.as_str().to_owned(), serde_json::json!({"narHash": "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=", "narSize": 1, "references": references, "signatures": vec!["s".repeat(256); 64]}))).collect();
        let bytes = serde_json::to_vec(&rows).unwrap();
        let observed = path_info(&bytes, &paths).unwrap();
        assert_eq!(observed.len(), 64);
        assert!(observed.values().all(|row| row.references.len() == 512));
    }

    #[test]
    fn hash_conversion_rejects_truncation_extra_rows_and_wrong_alphabet() {
        let good = format!("{}\n", "0".repeat(52));
        assert_eq!(
            converted_hashes(good.as_bytes(), &["hash"]).unwrap()["hash"],
            "0".repeat(52)
        );
        for bad in [
            good.trim_end().to_owned(),
            format!("{good}{good}"),
            format!("sha256:{good}"),
            format!("{}\n", "e".repeat(52)),
        ] {
            assert!(converted_hashes(bad.as_bytes(), &["hash"]).is_err());
        }
    }
}
