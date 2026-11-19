use serde::{Deserialize, Serialize};
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{self, Read, Seek, Write};
use std::mem::MaybeUninit;
use std::os::fd::FromRawFd;
use std::process::{Command, Stdio};
use std::ptr;
use std::time::Duration;

const CALLER_ACCESS_ARGUMENT: &str = "--neo-policy-caller-access";
const MAX_REQUEST_BYTES: u64 = 4 * 1024 * 1024;

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(in crate::installation) struct StockCaller {
    uid: u32,
    gid: u32,
}

impl StockCaller {
    pub(in crate::installation) fn new(uid: u32, gid: u32) -> Self {
        Self { uid, gid }
    }

    pub(super) fn current() -> Self {
        Self::new(unsafe { libc::geteuid() }, unsafe { libc::getegid() })
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub(super) enum CallerAccess {
    Allowed,
    Denied,
    #[default]
    Unproved,
}

#[derive(Deserialize, PartialEq, Eq, Serialize)]
#[serde(deny_unknown_fields)]
pub(super) struct DaemonAccessPolicy {
    pub(super) allowed_users: Vec<String>,
    pub(super) trusted_users: Vec<String>,
    pub(super) build_users_group: String,
}

impl Default for DaemonAccessPolicy {
    fn default() -> Self {
        Self {
            allowed_users: vec!["*".into()],
            trusted_users: vec!["root".into()],
            build_users_group: "nixbld".into(),
        }
    }
}

impl DaemonAccessPolicy {
    pub(super) fn access(&self, caller: StockCaller) -> CallerAccess {
        match self.probe(caller).as_deref() {
            Some([1]) => CallerAccess::Allowed,
            Some([0]) => CallerAccess::Denied,
            _ => CallerAccess::Unproved,
        }
    }

    fn probe(&self, caller: StockCaller) -> Option<Vec<u8>> {
        let descriptor =
            unsafe { libc::memfd_create(c"neo-caller-access".as_ptr(), libc::MFD_CLOEXEC) };
        if descriptor < 0 {
            return None;
        }
        let mut request = unsafe { File::from_raw_fd(descriptor) };
        serde_json::to_writer(&mut request, &(caller, self)).ok()?;
        if request.rewind().is_err() {
            return None;
        }
        let mut command = Command::new("/proc/self/exe");
        command
            .arg(CALLER_ACCESS_ARGUMENT)
            .stdin(Stdio::from(request));
        super::policy_observation::bounded_output(command, Duration::from_millis(400)).ok()
    }

    fn permits(&self, caller: StockCaller) -> io::Result<bool> {
        let user = lookup(
            |record, buffer, len, result| unsafe {
                libc::getpwuid_r(caller.uid, record, buffer, len, result)
            },
            |record| unsafe { CStr::from_ptr(record.pw_name).to_bytes().to_vec() },
        )?
        .unwrap_or_else(|| caller.uid.to_string().into_bytes());
        let group = lookup(
            |record, buffer, len, result| unsafe {
                libc::getgrgid_r(caller.gid, record, buffer, len, result)
            },
            |record| unsafe { CStr::from_ptr(record.gr_name).to_bytes().to_vec() },
        )?
        .unwrap_or_else(|| caller.gid.to_string().into_bytes());
        evaluate(self, &user, &group, |name| group_contains(name, &user))
    }
}

pub(crate) fn dispatch_caller_access(arguments: &[std::ffi::OsString]) -> Option<i32> {
    if arguments != [CALLER_ACCESS_ARGUMENT] {
        return None;
    }
    Some(i32::from(run_caller_access().is_err()))
}

fn run_caller_access() -> io::Result<()> {
    let mut bytes = Vec::new();
    io::stdin()
        .take(MAX_REQUEST_BYTES + 1)
        .read_to_end(&mut bytes)?;
    if bytes.len() as u64 > MAX_REQUEST_BYTES {
        return Err(io::Error::other("caller access request exceeds limit"));
    }
    let (caller, policy): (StockCaller, DaemonAccessPolicy) = serde_json::from_slice(&bytes)?;
    let allowed = policy.permits(caller)?;
    io::stdout().write_all(&[u8::from(allowed)])
}

fn evaluate(
    policy: &DaemonAccessPolicy,
    user: &[u8],
    group: &[u8],
    supplementary: impl Fn(&str) -> io::Result<bool>,
) -> io::Result<bool> {
    if group == policy.build_users_group.as_bytes() {
        return Ok(false);
    }
    Ok(matches(&policy.trusted_users, user, group, &supplementary)?
        || matches(&policy.allowed_users, user, group, &supplementary)?)
}

fn matches(
    rules: &[String],
    user: &[u8],
    group: &[u8],
    supplementary: &impl Fn(&str) -> io::Result<bool>,
) -> io::Result<bool> {
    if rules
        .iter()
        .any(|rule| rule == "*" || rule.as_bytes() == user)
    {
        return Ok(true);
    }
    for rule in rules {
        if let Some(name) = rule.strip_prefix('@') {
            if name.as_bytes() == group || supplementary(name)? {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

fn group_contains(name: &str, user: &[u8]) -> io::Result<bool> {
    let name = CString::new(name)?;
    Ok(lookup(
        |record, buffer, len, result| unsafe {
            libc::getgrnam_r(name.as_ptr(), record, buffer, len, result)
        },
        |record| {
            let mut member = record.gr_mem;
            while !member.is_null() && unsafe { !(*member).is_null() } {
                if unsafe { CStr::from_ptr(*member) }.to_bytes() == user {
                    return true;
                }
                member = unsafe { member.add(1) };
            }
            false
        },
    )?
    .unwrap_or(false))
}

fn lookup<T, R>(
    call: impl Fn(*mut T, *mut libc::c_char, usize, *mut *mut T) -> libc::c_int,
    read: impl FnOnce(&T) -> R,
) -> io::Result<Option<R>> {
    let mut storage = vec![0u8; 4096];
    loop {
        let mut record = MaybeUninit::<T>::uninit();
        let mut found = ptr::null_mut();
        let status = call(
            record.as_mut_ptr(),
            storage.as_mut_ptr().cast(),
            storage.len(),
            &mut found,
        );
        match status {
            0 => return Ok(unsafe { found.as_ref() }.map(read)),
            libc::ERANGE if storage.len() < 1024 * 1024 => storage.resize(storage.len() * 2, 0),
            error => return Err(io::Error::from_raw_os_error(error)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn timed_out_account_lookup_process_is_reaped() {
        let scratch = tempfile::tempdir().unwrap();
        let pid_file = scratch.path().join("worker.pid");
        let mut command = Command::new("/bin/sh");
        command
            .args([
                "-c",
                "printf '%s\\n' \"$$\" > \"$1\"; exec /bin/sleep 30",
                "account-probe",
            ])
            .arg(&pid_file)
            .stdin(Stdio::null());
        assert!(
            super::super::policy_observation::bounded_output(command, Duration::from_millis(200),)
                .is_err()
        );
        let pid: i32 = std::fs::read_to_string(pid_file)
            .unwrap()
            .trim()
            .parse()
            .unwrap();
        assert_eq!(unsafe { libc::kill(pid, 0) }, -1);
        assert_eq!(io::Error::last_os_error().raw_os_error(), Some(libc::ESRCH));
    }

    #[test]
    fn pinned_stock_admission_covers_trust_groups_and_build_user_exclusion() {
        let mut policy = DaemonAccessPolicy::default();
        let member = |group: &str| Ok(group == "builders");
        assert!(evaluate(&policy, b"jess", b"users", member).unwrap());
        assert!(!evaluate(&policy, b"jess", b"nixbld", member).unwrap());
        policy.allowed_users = vec!["@staff".into(), "@builders".into()];
        assert!(evaluate(&policy, b"jess", b"staff", |_| Ok(false)).unwrap());
        assert!(evaluate(&policy, b"jess", b"users", member).unwrap());
        assert!(!evaluate(&policy, b"jess", b"users", |_| Ok(false)).unwrap());
        policy.allowed_users.clear();
        policy.trusted_users.push("jess".into());
        assert!(evaluate(&policy, b"jess", b"users", member).unwrap());
        policy.trusted_users.push("*".into());
        assert!(!evaluate(&policy, b"jess", b"nixbld", member).unwrap());
    }
}
