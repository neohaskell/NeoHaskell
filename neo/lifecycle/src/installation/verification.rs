use super::{InstallationOwner, PairIdentity, TransactionError, nix, process};
use crate::candidate::{self, NarMetadata, OutputPath, ParsedCandidate};
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

pub(super) struct RegisteredRoot {
    pair: OutputPath,
    root: PathBuf,
}

impl InstallationOwner {
    pub(super) async fn observe(
        &self,
        lock: &super::Effects,
        operation: process::WorkerRequest,
    ) -> Result<process::OperationResult, TransactionError> {
        lock.process(operation).await
    }

    pub(super) async fn successful_observation(
        &self,
        lock: &super::Effects,
        operation: process::WorkerRequest,
    ) -> Result<Vec<u8>, TransactionError> {
        let result = self.observe(lock, operation).await?;
        if result.exit_code != Some(0) {
            return Err(TransactionError::Verification("Nix operation failed"));
        }
        Ok(result.stdout)
    }

    pub(super) async fn metadata_observations(
        &self,
        lock: &super::Effects,
        cache: Option<usize>,
        expected: &[(&OutputPath, &NarMetadata)],
    ) -> Result<(), TransactionError> {
        let mut remaining = expected;
        while !remaining.is_empty() {
            let mut count = remaining.len().min(64);
            let (operation, paths) = loop {
                let paths: Vec<_> = remaining[..count]
                    .iter()
                    .map(|(path, _)| (*path).clone())
                    .collect();
                let operation = process::WorkerRequest::PathInfo {
                    cache,
                    paths: paths.iter().map(|path| path.as_str().into()).collect(),
                };
                #[cfg(test)]
                let operation = if self.fixture && cache == Some(0) {
                    process::WorkerRequest::FixturePathInfo {
                        paths: paths.iter().map(|path| path.as_str().into()).collect(),
                    }
                } else {
                    operation
                };
                if operation.fits(&self.layout.state_directory) {
                    break (operation, paths);
                }
                if count == 1 {
                    return Err(TransactionError::Verification("metadata request budget"));
                }
                count /= 2;
            };
            let bytes = self.successful_observation(lock, operation).await?;
            let observed = nix::path_info(&bytes, &paths)
                .map_err(|_| TransactionError::Verification("path metadata response"))?;
            for (path, expected) in &remaining[..count] {
                let actual = &observed[*path];
                let references: BTreeSet<_> = expected.references().iter().cloned().collect();
                if actual.hash != expected.hash()
                    || actual.size != expected.size()
                    || actual.references != references
                {
                    return Err(TransactionError::Verification(
                        "candidate NAR identity mismatch",
                    ));
                }
            }
            remaining = &remaining[count..];
        }
        Ok(())
    }

    pub(super) async fn verify_remote(
        &self,
        lock: &super::Effects,
        candidate: &ParsedCandidate,
        system: &str,
        query_caches: bool,
    ) -> Result<(), TransactionError> {
        let selected = candidate
            .selected(system)
            .map_err(|_| TransactionError::Verification("candidate system"))?;
        let mut converted = BTreeMap::new();
        let hashes: Vec<_> = selected
            .closure()
            .values()
            .map(NarMetadata::hash)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        let mut remaining = hashes.as_slice();
        while !remaining.is_empty() {
            let mut count = remaining.len().min(64);
            let operation = loop {
                let operation = process::WorkerRequest::HashConvert {
                    hashes: remaining[..count]
                        .iter()
                        .map(|hash| (*hash).into())
                        .collect(),
                };
                if operation.fits(&self.layout.state_directory) {
                    break operation;
                }
                if count == 1 {
                    return Err(TransactionError::Verification("hash request budget"));
                }
                count /= 2;
            };
            let bytes = self.successful_observation(lock, operation).await?;
            converted.extend(
                nix::converted_hashes(&bytes, &remaining[..count])
                    .map_err(|_| TransactionError::Verification("hash conversion response"))?,
            );
            remaining = &remaining[count..];
        }
        let mut caches = BTreeMap::<usize, Vec<(&OutputPath, &NarMetadata)>>::new();
        for (path, metadata) in selected.closure() {
            for witness in metadata.locations() {
                witness
                    .verify(path, metadata, &converted[metadata.hash()])
                    .map_err(|_| TransactionError::Verification("frozen cache signature"))?;
                let mut index = 0;
                loop {
                    let declared = candidate::declared_cache(index)
                        .map_err(|_| TransactionError::Verification("declared cache"))?;
                    if declared.url() == witness.cache().url() {
                        break;
                    }
                    index += 1;
                }
                caches.entry(index).or_default().push((path, metadata));
            }
        }
        if query_caches {
            for (cache, expected) in caches {
                self.metadata_observations(lock, Some(cache), &expected)
                    .await?;
            }
        }
        Ok(())
    }

    pub(super) async fn verify_retention_observation(
        &self,
        lock: &super::Effects,
        candidate: &ParsedCandidate,
        system: &str,
        retained: super::resource_io::RetentionObservation,
    ) -> Result<PairIdentity, TransactionError> {
        let registered = RegisteredRoot {
            pair: retained.pair().clone(),
            root: retained.root().to_owned(),
        };
        self.verify_retained(lock, candidate, system, &registered)
            .await
    }

    pub(super) async fn verify_retained(
        &self,
        lock: &super::Effects,
        candidate: &ParsedCandidate,
        system: &str,
        retained: &RegisteredRoot,
    ) -> Result<PairIdentity, TransactionError> {
        let selected = candidate
            .selected(system)
            .map_err(|_| TransactionError::Verification("candidate system"))?;
        if selected.output("pair") != Some(&retained.pair)
            || self.inspect_link(retained.root.clone()).await?.as_deref()
                != Some(std::path::Path::new(retained.pair.as_str()))
        {
            return Err(TransactionError::Verification("retained pair identity"));
        }
        self.metadata_observations(lock, None, &selected.closure().iter().collect::<Vec<_>>())
            .await?;
        self.successful_observation(
            lock,
            process::WorkerRequest::VerifyContents {
                pair: retained.pair.as_str().into(),
            },
        )
        .await?;
        for name in ["neo", "neoctl"] {
            let output = selected
                .output(name)
                .ok_or(TransactionError::Verification("pair member"))?;
            let member = PathBuf::from(retained.pair.as_str()).join("bin").join(name);
            let expected = PathBuf::from(output.as_str()).join("bin").join(name);
            self.effects
                .filesystem(super::FilesystemJob::ResolvePairMember { member, expected })
                .await?;
            let result = self
                .observe(
                    lock,
                    process::WorkerRequest::ProbeTool {
                        output: output.as_str().into(),
                        tool: if name == "neo" {
                            process::Tool::Neo
                        } else {
                            process::Tool::Neoctl
                        },
                        probe: process::Probe::Version,
                    },
                )
                .await?;
            if result.exit_code != Some(0)
                || result.stdout != format!("{name} {}\n", candidate.version()).as_bytes()
                || !result.stderr.is_empty()
            {
                return Err(TransactionError::Verification("tool version behavior"));
            }
        }
        let neoctl = selected
            .output("neoctl")
            .ok_or(TransactionError::Verification("neoctl output"))?;
        for probe in [
            process::Probe::NoArguments,
            process::Probe::Help,
            process::Probe::ShortHelp,
            process::Probe::UnknownFlag,
            process::Probe::Doctor,
            process::Probe::MixedHelp,
        ] {
            let valid = matches!(
                probe,
                process::Probe::NoArguments | process::Probe::Help | process::Probe::ShortHelp
            );
            let result = self
                .observe(
                    lock,
                    process::WorkerRequest::ProbeTool {
                        output: neoctl.as_str().into(),
                        tool: process::Tool::Neoctl,
                        probe,
                    },
                )
                .await?;
            let usage = b"Usage: neoctl [--help|-h|--version]\nDisplay usage and packaged version information for neoctl.\n";
            let error = b"Invalid arguments. Run neoctl --help for usage.\n";
            if result.exit_code != Some(if valid { 0 } else { 2 })
                || result.stdout != if valid { usage.as_slice() } else { &[] }
                || result.stderr != if valid { &[] } else { error.as_slice() }
            {
                return Err(TransactionError::Verification("neoctl argument behavior"));
            }
        }
        Ok(PairIdentity {
            version: candidate.version().into(),
            output: PathBuf::from(retained.pair.as_str()),
            selection: Some(super::release::PinnedSelection {
                version: candidate.version().into(),
                candidate_id: candidate.id().into(),
            }),
        })
    }
}
