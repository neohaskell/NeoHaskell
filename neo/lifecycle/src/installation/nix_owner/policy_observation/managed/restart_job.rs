use super::{
    CommandOutput, MANAGER_READ_BUDGET, ManagedSnapshot, PolicyResult, bounded_output,
    protected_path, read_command_output, systemctl,
};
use serde::Deserialize;
use std::num::NonZeroU32;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Duration;

const DAEMON_SERVICE: &str = "nix-daemon.service";
const DAEMON_SOCKET: &str = "nix-daemon.socket";
const RESTART_BUDGET: Duration = Duration::from_secs(5);
const ANCHOR_PREFIX: &[u8] = b"Enqueued anchor job ";
const ANCHOR_SUFFIX: &[u8] = b" nix-daemon.service/restart.\n";
const UNIT_STOPPED: &str = "9d1aaa27d60140bd96365438aad20286";
const UNIT_STARTED: &str = "39f53479d3a045ac8e11786248231fbf";

pub(super) fn request_restart(
    acknowledgement: Option<&mut dyn FnMut(NonZeroU32) -> Result<(), PolicyResult>>,
) -> Result<(), PolicyResult> {
    let executable = systemctl()?;
    let mut command = Command::new(&executable);
    command.arg("--no-ask-password").stdin(Stdio::null());
    if let Some(record) = acknowledgement.filter(|_| supports_transaction(&executable)) {
        command.args(["--show-transaction", "restart", DAEMON_SERVICE]);
        record_acknowledgement(command, record)
    } else {
        command.args(["restart", DAEMON_SERVICE]);
        bounded_output(command, RESTART_BUDGET).map(|_| ())
    }
}

fn supports_transaction(executable: &Path) -> bool {
    let mut command = Command::new(executable);
    command.arg("--help").stdin(Stdio::null());
    bounded_output(command, MANAGER_READ_BUDGET).is_ok_and(|help| {
        help.split(|byte| byte.is_ascii_whitespace())
            .any(|word| word == b"--show-transaction")
    })
}

fn record_acknowledgement(
    command: Command,
    record: &mut dyn FnMut(NonZeroU32) -> Result<(), PolicyResult>,
) -> Result<(), PolicyResult> {
    let mut anchor = RestartAnchor::default();
    let mut record = Some(record);
    read_command_output(
        command,
        RESTART_BUDGET,
        CommandOutput::RestartAcknowledgement,
        |chunk| {
            if record.is_some()
                && let Some(job_id) = anchor.read_chunk(chunk)
                && let Some(write_job_id) = record.take()
            {
                write_job_id(job_id)?;
            }
            Ok(())
        },
    )
}

#[derive(Default)]
struct RestartAnchor {
    line: Vec<u8>,
}

impl RestartAnchor {
    fn read_chunk(&mut self, chunk: &[u8]) -> Option<NonZeroU32> {
        for fragment in chunk.split_inclusive(|byte| *byte == b'\n') {
            self.line.extend_from_slice(fragment);
            if self.line.last() == Some(&b'\n') {
                let id = anchor_job_id(&self.line);
                self.line.clear();
                if id.is_some() {
                    return id;
                }
            }
        }
        None
    }
}

fn anchor_job_id(line: &[u8]) -> Option<NonZeroU32> {
    let id = line
        .strip_prefix(ANCHOR_PREFIX)?
        .strip_suffix(ANCHOR_SUFFIX)?;
    if id.first() == Some(&b'0') || !id.iter().all(u8::is_ascii_digit) {
        return None;
    }
    std::str::from_utf8(id).ok()?.parse().ok()
}

pub(in super::super) fn verify_final_restart(
    boot_id: &str,
    job_id: NonZeroU32,
    old_invocation: &str,
    intent_monotonic: u64,
    candidate: &ManagedSnapshot,
) -> Result<(), PolicyResult> {
    let boot = boot_id.replace('-', "");
    let new_invocation = format!("{:032x}", candidate.invocation);
    if boot.len() != 32
        || !boot.bytes().all(|byte| byte.is_ascii_hexdigit())
        || old_invocation == new_invocation
        || intent_monotonic >= candidate.started
    {
        return Err(PolicyResult::UnprovedService);
    }
    verify_no_daemon_jobs()?;
    let journalctl = protected_path(Path::new("/usr/bin/journalctl"))
        .map_err(|_| PolicyResult::UnprovedService)?;
    let mut command = Command::new(journalctl);
    command
        .args([
            "--system",
            "--no-pager",
            "--output=json",
            "--output-fields=_PID,_UID,_TRANSPORT,UNIT,MESSAGE_ID,JOB_TYPE,JOB_RESULT,JOB_ID,INVOCATION_ID",
            "--lines=3",
            "_PID=1",
            "_UID=0",
            "_TRANSPORT=journal",
            "JOB_RESULT=done",
        ])
        .arg(format!("_BOOT_ID={boot}"))
        .arg(format!("UNIT={DAEMON_SERVICE}"))
        .arg(format!("JOB_ID={job_id}"))
        .stdin(Stdio::null());
    let output = bounded_output(command, MANAGER_READ_BUDGET)?;
    verify_job_entries(
        &output,
        &boot,
        job_id,
        old_invocation,
        &new_invocation,
        intent_monotonic,
        candidate.started,
    )?;
    verify_no_daemon_jobs()
}

fn verify_no_daemon_jobs() -> Result<(), PolicyResult> {
    let mut command = Command::new(systemctl()?);
    command
        .args([
            "list-jobs",
            "--no-pager",
            "--no-legend",
            DAEMON_SERVICE,
            DAEMON_SOCKET,
        ])
        .stdin(Stdio::null());
    require_empty_job_list(command)
}

fn require_empty_job_list(command: Command) -> Result<(), PolicyResult> {
    if !bounded_output(command, MANAGER_READ_BUDGET)?.is_empty() {
        return Err(PolicyResult::UnprovedService);
    }
    Ok(())
}

#[derive(Deserialize)]
struct JobEntry<'a> {
    #[serde(rename = "_BOOT_ID")]
    boot: &'a str,
    #[serde(rename = "_PID")]
    pid: &'a str,
    #[serde(rename = "_UID")]
    uid: &'a str,
    #[serde(rename = "_TRANSPORT")]
    transport: &'a str,
    #[serde(rename = "UNIT")]
    unit: &'a str,
    #[serde(rename = "MESSAGE_ID")]
    message_id: &'a str,
    #[serde(rename = "JOB_ID")]
    job_id: &'a str,
    #[serde(rename = "JOB_TYPE")]
    kind: &'a str,
    #[serde(rename = "JOB_RESULT")]
    result: &'a str,
    #[serde(rename = "INVOCATION_ID")]
    invocation: &'a str,
    #[serde(rename = "__MONOTONIC_TIMESTAMP")]
    monotonic_timestamp: &'a str,
}

fn verify_job_entries(
    output: &[u8],
    boot: &str,
    job_id: NonZeroU32,
    old_invocation: &str,
    new_invocation: &str,
    intent: u64,
    new_started: u64,
) -> Result<(), PolicyResult> {
    let output = output
        .strip_suffix(b"\n")
        .ok_or(PolicyResult::UnprovedService)?;
    let mut entries = output.split(|byte| *byte == b'\n');
    let job_id = job_id.to_string();
    let mut next = || {
        let entry: JobEntry<'_> =
            serde_json::from_slice(entries.next().ok_or(PolicyResult::UnprovedService)?)
                .map_err(|_| PolicyResult::UnprovedService)?;
        if entry.boot != boot
            || entry.pid != "1"
            || entry.uid != "0"
            || entry.transport != "journal"
            || entry.unit != DAEMON_SERVICE
            || entry.job_id != job_id
            || entry.result != "done"
        {
            return Err(PolicyResult::UnprovedService);
        }
        let timestamp = entry
            .monotonic_timestamp
            .parse::<u64>()
            .map_err(|_| PolicyResult::UnprovedService)?;
        Ok((entry, timestamp))
    };
    let (stopped, stop_time) = next()?;
    let (started, start_time) = next()?;
    if entries.next().is_some()
        || stopped.message_id != UNIT_STOPPED
        || stopped.kind != "restart"
        || stopped.invocation != old_invocation
        || started.message_id != UNIT_STARTED
        || started.kind != "start"
        || started.invocation != new_invocation
        || stop_time <= intent
        || stop_time >= start_time
        || start_time < new_started
    {
        return Err(PolicyResult::UnprovedService);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const BOOT: &str = "d5cea6792f7e4c9080b5ff62358185cb";
    const OLD: &str = "ed818badcefa48bcb940eac5abdc16ca";
    const REPLACEMENT: &str = "df223a97a58d4581815a6562c5f4c92d";
    const JOURNAL: &str = concat!(
        r#"{"_BOOT_ID":"d5cea6792f7e4c9080b5ff62358185cb","_PID":"1","_UID":"0","_TRANSPORT":"journal","UNIT":"nix-daemon.service","MESSAGE_ID":"9d1aaa27d60140bd96365438aad20286","JOB_ID":"269","JOB_TYPE":"restart","JOB_RESULT":"done","INVOCATION_ID":"ed818badcefa48bcb940eac5abdc16ca","__MONOTONIC_TIMESTAMP":"1694081077397"}"#,
        "\n",
        r#"{"_BOOT_ID":"d5cea6792f7e4c9080b5ff62358185cb","_PID":"1","_UID":"0","_TRANSPORT":"journal","UNIT":"nix-daemon.service","MESSAGE_ID":"39f53479d3a045ac8e11786248231fbf","JOB_ID":"269","JOB_TYPE":"start","JOB_RESULT":"done","INVOCATION_ID":"df223a97a58d4581815a6562c5f4c92d","__MONOTONIC_TIMESTAMP":"1694081083650"}"#,
        "\n",
    );

    #[test]
    fn recognizes_only_a_complete_anchor_for_the_fixed_restart() {
        let anchor = b"Enqueued anchor job 269 nix-daemon.service/restart.\n";
        assert_eq!(anchor_job_id(anchor), NonZeroU32::new(269));
        for other in [
            &anchor[..anchor.len() - 1],
            b"Enqueued auxiliary job 269 nix-daemon.service/restart.\n",
            b"Enqueued anchor job 269 nix-daemon.socket/restart.\n",
            b"Enqueued anchor job 269 nix-daemon.service/start.\n",
            b"Enqueued anchor job 0 nix-daemon.service/restart.\n",
            b"Enqueued anchor job 0269 nix-daemon.service/restart.\n",
            b"Enqueued anchor job +269 nix-daemon.service/restart.\n",
        ] {
            assert_eq!(anchor_job_id(other), None);
        }
        for split in 0..anchor.len() {
            let mut reader = RestartAnchor::default();
            assert_eq!(reader.read_chunk(&anchor[..split]), None);
            assert_eq!(reader.read_chunk(&anchor[split..]), NonZeroU32::new(269));
        }
    }

    #[test]
    fn requires_successful_and_empty_list_of_daemon_jobs() {
        let command = |script| {
            let mut command = Command::new("/bin/sh");
            command.args(["-c", script]).stdin(Stdio::null());
            command
        };
        assert_eq!(require_empty_job_list(command("exit 0")), Ok(()));
        for result in [
            "exit 1",
            "echo '269 nix-daemon.service restart running'",
            "echo '270 nix-daemon.socket stop waiting'",
        ] {
            assert!(require_empty_job_list(command(result)).is_err());
        }
    }

    fn check(journal: &str, id: u32, replacement: &str) -> Result<(), PolicyResult> {
        verify_job_entries(
            journal.as_bytes(),
            BOOT,
            NonZeroU32::new(id).unwrap(),
            OLD,
            replacement,
            1_694_081_076_036,
            1_694_081_083_526,
        )
    }

    #[test]
    fn authenticates_one_completed_restart_for_the_expected_invocations() {
        assert_eq!(check(JOURNAL, 269, REPLACEMENT), Ok(()));
        assert!(check(JOURNAL, 268, REPLACEMENT).is_err());
        assert!(check(JOURNAL, 269, OLD).is_err());
        assert!(check("", 269, REPLACEMENT).is_err());
        let stop_only = JOURNAL.split_inclusive('\n').next().unwrap();
        assert!(check(stop_only, 269, REPLACEMENT).is_err());
        let start_only = JOURNAL.strip_prefix(stop_only).unwrap();
        assert!(check(start_only, 269, REPLACEMENT).is_err());
        assert!(check(&format!("{JOURNAL}{start_only}"), 269, REPLACEMENT).is_err());
        for invalid in [
            JOURNAL.replacen("\"done\"", "\"canceled\"", 1),
            JOURNAL.replacen("\"_UID\":\"0\"", "\"_UID\":\"1000\"", 1),
            JOURNAL.replacen("\"_PID\":\"1\"", "\"_PID\":\"2\"", 1),
            JOURNAL.replacen(BOOT, "05cea6792f7e4c9080b5ff62358185cb", 1),
            JOURNAL.replacen("1694081077397", "1694081076035", 1),
            JOURNAL.replacen("1694081083650", "1694081083525", 1),
            JOURNAL.replacen("\"269\"", "[\"269\",\"270\"]", 1),
        ] {
            assert!(check(&invalid, 269, REPLACEMENT).is_err());
        }
    }
}
