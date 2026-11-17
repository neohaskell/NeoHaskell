use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct LinkObservation {
    pub(super) device: u64,
    pub(super) inode: u64,
    pub(super) uid: u32,
    pub(super) target: String,
}

pub(super) type Inventory = BTreeMap<String, LinkObservation>;

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) enum ResourceEffect {
    Acquire {
        pair: String,
        root: String,
    },
    Register {
        pair: String,
        parent: String,
    },
    Remove {
        name: String,
        observed: LinkObservation,
    },
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ParentObservation {
    pub(super) device: u64,
    pub(super) inode: u64,
    pub(super) uid: u32,
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ResourceLedger {
    pub(super) links: Inventory,
    pub(super) parent: Option<ParentObservation>,
    pub(super) pending: Option<ResourceEffect>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) enum ResourceError {
    Foreign,
    PendingEffect,
    InvalidProfile,
}

fn temporary_of(name: &str, parent: &str) -> bool {
    let Some(suffix) = name
        .strip_prefix(parent)
        .and_then(|rest| rest.strip_prefix(".tmp-"))
    else {
        return false;
    };
    let Some((pid, random)) = suffix.split_once('-') else {
        return false;
    };
    !pid.is_empty()
        && !random.is_empty()
        && pid.bytes().all(|b| b.is_ascii_digit())
        && random.bytes().all(|b| b.is_ascii_digit())
}

fn generation(name: &str) -> Option<&str> {
    let stem = name.split_once(".tmp-").map_or(name, |(stem, _)| stem);
    let number = stem.strip_prefix("prepared-")?.strip_suffix("-link")?;
    if number.is_empty()
        || !number.bytes().all(|b| b.is_ascii_digit())
        || (name != stem && !temporary_of(name, stem))
    {
        return None;
    }
    Some(stem)
}

pub(super) fn is_generation(name: &str) -> bool {
    generation(name).is_some()
}

fn switch_temporary(name: &str) -> bool {
    name.strip_prefix('.')
        .and_then(|rest| rest.strip_suffix("_prepared"))
        .is_some_and(|number| !number.is_empty() && number.bytes().all(|b| b.is_ascii_digit()))
}

impl ResourceLedger {
    pub(super) fn begin(
        &mut self,
        effect: ResourceEffect,
        observed: &Inventory,
    ) -> Result<(), ResourceError> {
        if self.pending.is_some() {
            return Err(ResourceError::PendingEffect);
        }
        if &self.links != observed {
            return Err(ResourceError::Foreign);
        }
        self.pending = Some(effect);
        Ok(())
    }

    pub(super) fn reconcile(&mut self, observed: Inventory, uid: u32) -> Result<(), ResourceError> {
        let mut baseline = self.links.clone();
        baseline.retain(|name, _| observed.contains_key(name) || !name.starts_with("candidate-") || matches!(&self.pending, Some(ResourceEffect::Remove { name: removing, .. }) if removing == name));
        let Some(effect) = &self.pending else {
            if baseline != observed {
                return Err(ResourceError::Foreign);
            }
            self.links = observed;
            return Ok(());
        };
        if observed.values().any(|link| link.uid != uid) {
            return Err(ResourceError::Foreign);
        }
        match effect {
            ResourceEffect::Remove {
                name,
                observed: expected,
            } => {
                let mut after = baseline.clone();
                if after.get(name) != Some(expected) {
                    return Err(ResourceError::Foreign);
                }
                if !observed.contains_key(name) {
                    after.remove(name);
                }
                if after != observed {
                    return Err(ResourceError::Foreign);
                }
            }
            ResourceEffect::Register { pair, parent } => {
                let mut children = 0;
                for (name, old) in &baseline {
                    let current = observed.get(name).ok_or(ResourceError::Foreign)?;
                    if current != old
                        && !(name == parent
                            && current.target == *pair
                            && old.target == *pair
                            && current.uid == old.uid
                            && current.device == old.device)
                    {
                        return Err(ResourceError::Foreign);
                    }
                }
                for (name, current) in &observed {
                    if baseline.contains_key(name) {
                        continue;
                    }
                    if name == parent && current.target == *pair {
                        continue;
                    }
                    if !temporary_of(name, parent) || current.target != *pair {
                        return Err(ResourceError::Foreign);
                    }
                    children += 1;
                }
                if children > 1 {
                    return Err(ResourceError::Foreign);
                }
            }
            ResourceEffect::Acquire { pair, root } => {
                let mut stems = BTreeSet::new();
                let mut generations = 0;
                let mut generation_temporaries = 0;
                let mut root_temporaries = 0;
                let mut switches = 0;
                for (name, old) in &baseline {
                    let current = observed.get(name).ok_or(ResourceError::Foreign)?;
                    if old == current {
                        continue;
                    }
                    let target_write = (name == root || generation(name).is_some())
                        && old.target == *pair
                        && current.target == *pair;
                    if !(name == "prepared" || target_write)
                        || current.uid != old.uid
                        || current.device != old.device
                    {
                        return Err(ResourceError::Foreign);
                    }
                }
                for (name, current) in &observed {
                    if baseline.contains_key(name) {
                        continue;
                    }
                    if name == "prepared" {
                        continue;
                    }
                    if name == root && current.target == *pair {
                        continue;
                    }
                    if temporary_of(name, root) && current.target == *pair {
                        root_temporaries += 1;
                        continue;
                    }
                    if switch_temporary(name) {
                        switches += 1;
                        continue;
                    }
                    let Some(stem) = generation(name) else {
                        return Err(ResourceError::Foreign);
                    };
                    if current.target != *pair {
                        return Err(ResourceError::Foreign);
                    }
                    stems.insert(stem);
                    if stem == name {
                        generations += 1;
                    } else {
                        generation_temporaries += 1;
                    }
                }
                if stems.len() > 1
                    || generations > 1
                    || generation_temporaries > 1
                    || root_temporaries > 1
                    || switches > 1
                {
                    return Err(ResourceError::Foreign);
                }
                for (name, current) in &observed {
                    if name != "prepared" && !switch_temporary(name) {
                        continue;
                    }
                    if generation(&current.target).is_none() {
                        return Err(ResourceError::InvalidProfile);
                    }
                    let target = observed
                        .get(&current.target)
                        .ok_or(ResourceError::InvalidProfile)?;
                    let authorized =
                        target.target == *pair || baseline.get(&current.target) == Some(target);
                    if !authorized {
                        return Err(ResourceError::InvalidProfile);
                    }
                }
            }
        }
        self.links = observed;
        self.pending = None;
        Ok(())
    }

    pub(super) fn obsolete(
        &self,
        retained_roots: &BTreeSet<String>,
    ) -> Result<Vec<(String, LinkObservation)>, ResourceError> {
        if self.pending.is_some() {
            return Err(ResourceError::PendingEffect);
        }
        let selected = self.links.get("prepared").map(|link| link.target.as_str());
        if let Some(selected) = selected {
            if generation(selected).is_none() || !self.links.contains_key(selected) {
                return Err(ResourceError::InvalidProfile);
            }
        }
        Ok(self
            .links
            .iter()
            .filter(|(name, _)| {
                name.as_str() != "prepared"
                    && Some(name.as_str()) != selected
                    && !retained_roots.contains(*name)
            })
            .map(|(name, link)| (name.clone(), link.clone()))
            .collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn link(target: &str, inode: u64) -> LinkObservation {
        LinkObservation {
            device: 1,
            inode,
            uid: 1001,
            target: target.into(),
        }
    }

    #[test]
    fn completed_saved_profile_effects_preserve_exact_owned_inventory() {
        #[derive(Deserialize)]
        struct Case {
            name: String,
            pair: String,
            root: String,
            before: Inventory,
            after: Inventory,
        }
        #[derive(Deserialize)]
        struct Cases {
            cases: Vec<Case>,
        }
        let saved: Cases = serde_json::from_str(include_str!(
            "../../tests/fixtures/retained-profile-observations.json"
        ))
        .unwrap();
        assert!(!saved.cases.is_empty());
        for case in saved.cases {
            let mut ledger = ResourceLedger {
                links: case.before.clone(),
                pending: None,
                parent: None,
            };
            ledger
                .begin(
                    ResourceEffect::Acquire {
                        pair: case.pair,
                        root: case.root,
                    },
                    &case.before,
                )
                .unwrap();
            assert_eq!(ledger.reconcile(case.after, 1001), Ok(()), "{}", case.name);
        }
    }

    #[test]
    fn repeated_supported_writes_accept_inode_changes_but_preserve_retained_roots() {
        let baseline = Inventory::from([
            ("candidate-A".into(), link("A", 1)),
            ("candidate-B".into(), link("B", 2)),
            ("prepared".into(), link("prepared-2-link", 3)),
            ("prepared-2-link".into(), link("B", 4)),
        ]);
        let mut ledger = ResourceLedger {
            links: baseline.clone(),
            pending: None,
            parent: None,
        };
        ledger
            .begin(
                ResourceEffect::Acquire {
                    pair: "B".into(),
                    root: "candidate-B".into(),
                },
                &baseline,
            )
            .unwrap();
        let mut after = baseline.clone();
        after.get_mut("prepared").unwrap().inode = 30;
        after.get_mut("candidate-B").unwrap().inode = 20;
        ledger.reconcile(after.clone(), 1001).unwrap();
        assert!(
            ledger
                .obsolete(&BTreeSet::from([
                    "candidate-A".into(),
                    "candidate-B".into()
                ]))
                .unwrap()
                .is_empty()
        );
        ledger
            .begin(
                ResourceEffect::Acquire {
                    pair: "B".into(),
                    root: "candidate-B".into(),
                },
                &after,
            )
            .unwrap();
        after.get_mut("candidate-A").unwrap().inode = 10;
        assert_eq!(ledger.reconcile(after, 1001), Err(ResourceError::Foreign));
    }

    #[test]
    fn adopted_generation_registration_children_are_not_generation_candidates() {
        let parent = "prepared-1-link.tmp-6324-846930886";
        let baseline = Inventory::from([
            ("prepared".into(), link(parent, 1)),
            (parent.into(), link("B", 2)),
        ]);
        let mut ledger = ResourceLedger {
            links: baseline.clone(),
            pending: None,
            parent: None,
        };
        ledger
            .begin(
                ResourceEffect::Register {
                    pair: "B".into(),
                    parent: parent.into(),
                },
                &baseline,
            )
            .unwrap();
        let mut observed = baseline;
        let child = format!("{parent}.tmp-6383-1804289383");
        observed.insert(child.clone(), link("B", 3));
        ledger.reconcile(observed, 1001).unwrap();
        assert!(generation(&child).is_none());
        assert_eq!(
            ledger.obsolete(&BTreeSet::new()).unwrap(),
            vec![(child, link("B", 3))]
        );
    }
}
