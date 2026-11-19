use std::ffi::CStr;
use std::fs;
use std::io;
use std::mem::MaybeUninit;
use std::path::Path;
use std::ptr;

use super::configuration::protect_configuration_path;

const DAEMON_PROGRAM: &[u8] = b"/nix/var/nix/profiles/default/bin/nix-daemon";
const MAX_ARGUMENT_BYTES: usize = 256 * 1024;
const SECONDARY_CONFIGS: [&str; 2] = ["/var/root/.config/nix/nix.conf", "/etc/xdg/nix/nix.conf"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(super) enum StartupContext {
    RequiresPrivilege = 0,
    StandardSources = 1,
    Unproved = 2,
}

impl StartupContext {
    pub(super) fn observe(listener: super::RootPeer) -> Self {
        if unsafe { libc::geteuid() } != 0 {
            return Self::RequiresPrivilege;
        }
        let Some(arguments) = process_arguments(listener.pid()) else {
            return Self::Unproved;
        };
        if standard_arguments(&arguments)
            && standard_root_home()
            && SECONDARY_CONFIGS.into_iter().all(protected_absence)
        {
            Self::StandardSources
        } else {
            Self::Unproved
        }
    }

    pub(super) fn decode(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::RequiresPrivilege),
            1 => Some(Self::StandardSources),
            2 => Some(Self::Unproved),
            _ => None,
        }
    }
}

fn process_arguments(pid: u32) -> Option<Vec<u8>> {
    let mut mib = [
        libc::CTL_KERN,
        libc::KERN_PROCARGS2,
        i32::try_from(pid).ok()?,
    ];
    let mut length = 0;
    // KERN_PROCARGS2 reports the required size when the output pointer is null.
    let query = |output, length: &mut usize| unsafe {
        libc::sysctl(
            mib.as_mut_ptr(),
            mib.len() as libc::c_uint,
            output,
            length,
            ptr::null_mut(),
            0,
        )
    };
    let mut query = query;
    if query(ptr::null_mut(), &mut length) != 0 || !(4..=MAX_ARGUMENT_BYTES).contains(&length) {
        return None;
    }
    let expected_length = length;
    let mut arguments = vec![0; length];
    if query(arguments.as_mut_ptr().cast(), &mut length) != 0 || length != expected_length {
        return None;
    }
    Some(arguments)
}

fn standard_arguments(arguments: &[u8]) -> bool {
    let Some((count, strings)) = arguments.split_at_checked(4) else {
        return false;
    };
    if count != 4_i32.to_ne_bytes() || strings.last() != Some(&0) {
        return false;
    }
    let mut fields = strings.split(|byte| *byte == 0);
    if fields.next() != Some(DAEMON_PROGRAM) {
        return false;
    }
    let mut fields = fields.skip_while(|field| field.is_empty());
    let [program, flag, name, path] = std::array::from_fn(|_| fields.next());
    if program != Some(DAEMON_PROGRAM)
        || flag != Some(b"--option")
        || name != Some(b"json-log-path")
        || !path.is_some_and(|path| path.starts_with(b"/") && path.len() <= libc::PATH_MAX as usize)
    {
        return false;
    }
    // macOS can omit the environment and append separate Apple startup strings.
    let environment = fields.clone().take_while(|field| !field.is_empty());
    if !environment
        .clone()
        .any(|field| field.starts_with(b"XPC_SERVICE_NAME="))
        || !environment.clone().any(|field| field.starts_with(b"PATH="))
        || environment.clone().any(|field| !field.contains(&b'='))
    {
        return false;
    }
    fields.all(|field| {
        let Some(equal) = field.iter().position(|byte| *byte == b'=') else {
            return true;
        };
        let (key, value) = field.split_at(equal);
        match key {
            b"HOME" => matches!(&value[1..], b"/var/root" | b"/private/var/root"),
            b"NIX_CONFIG"
            | b"NIX_CONFIG_HOME"
            | b"NIX_CONF_DIR"
            | b"NIX_USER_CONF_FILES"
            | b"XDG_CONFIG_HOME"
            | b"XDG_CONFIG_DIRS"
            | b"NIX_STORE_DIR"
            | b"NIX_STATE_DIR"
            | b"NIX_ROOT"
            | b"NIX_REMOTE" => false,
            _ => !key.starts_with(b"DYLD_"),
        }
    })
}

fn standard_root_home() -> bool {
    let mut record = MaybeUninit::<libc::passwd>::uninit();
    let mut storage = [0; 16 * 1024];
    let mut result = ptr::null_mut();
    let status = unsafe {
        libc::getpwuid_r(
            0,
            record.as_mut_ptr(),
            storage.as_mut_ptr(),
            storage.len(),
            &mut result,
        )
    };
    if status != 0 || result != record.as_mut_ptr() {
        return false;
    }
    let home = unsafe { record.assume_init() }.pw_dir;
    !home.is_null()
        && matches!(
            unsafe { CStr::from_ptr(home) }.to_bytes(),
            b"/var/root" | b"/private/var/root"
        )
}

fn protected_absence(path: &str) -> bool {
    let path = Path::new(path);
    let missing = |path| matches!(fs::symlink_metadata(path), Err(error) if error.kind() == io::ErrorKind::NotFound);
    if !missing(path) {
        return false;
    }
    let Some(parent) = path.ancestors().skip(1).find(|entry| !missing(entry)) else {
        return false;
    };
    protect_configuration_path(parent)
        .ok()
        .is_some_and(|canonical| canonical.is_dir() && missing(path))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn frame(arguments: &[&[u8]], environment: &[&[u8]], apple: &[&[u8]]) -> Vec<u8> {
        let mut payload = (arguments.len() as i32).to_ne_bytes().to_vec();
        payload.extend_from_slice(DAEMON_PROGRAM);
        payload.extend_from_slice(&[0, 0, 0]);
        for field in arguments.iter().chain(environment) {
            payload.extend_from_slice(field);
            payload.push(0);
        }
        payload.push(0);
        for field in apple {
            payload.extend_from_slice(field);
            payload.push(0);
        }
        payload
    }

    #[test]
    fn launch_context_admits_only_the_observed_configuration_selectors() {
        let arguments = [
            DAEMON_PROGRAM,
            b"--option",
            b"json-log-path",
            b"/var/log/nix",
        ];
        let standard: [&[u8]; 2] = [
            b"XPC_SERVICE_NAME=systems.determinate.nix-daemon",
            b"PATH=/usr/bin:/bin",
        ];
        let apple: [&[u8]; 1] = [b"executable_path=/nix/var/nix/profiles/default/bin/nix-daemon"];
        assert!(standard_arguments(&frame(&arguments, &standard, &apple)));
        assert!(!standard_arguments(&frame(&arguments, &[], &apple)));
        assert!(!standard_arguments(&frame(
            &arguments[..3],
            &standard,
            &apple
        )));
        for prohibited in [
            b"NIX_CONFIG=ignored".as_slice(),
            b"NIX_CONFIG_HOME=/elsewhere",
            b"NIX_CONF_DIR=/elsewhere",
            b"NIX_USER_CONF_FILES=",
            b"HOME=/elsewhere",
            b"XDG_CONFIG_DIRS=/elsewhere",
            b"DYLD_INSERT_LIBRARIES=/tmp/foreign",
        ] {
            let overridden = [standard[0], standard[1], prohibited];
            assert!(!standard_arguments(&frame(&arguments, &overridden, &apple)));
            assert!(!standard_arguments(&frame(
                &arguments,
                &standard,
                &[prohibited]
            )));
        }
        let cli_override = [
            DAEMON_PROGRAM,
            b"--option",
            b"trusted-users",
            b"/tmp/foreign",
        ];
        assert!(!standard_arguments(&frame(
            &cli_override,
            &standard,
            &apple
        )));
    }
}
