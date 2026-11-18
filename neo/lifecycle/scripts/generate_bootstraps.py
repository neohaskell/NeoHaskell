#!/usr/bin/env python3
import argparse
from dataclasses import dataclass
import hashlib
import json
from pathlib import Path
import re
import shlex


TARGETS = {
    "Linux:x86_64": "x86_64-unknown-linux-gnu",
    "Linux:aarch64": "aarch64-unknown-linux-gnu",
    "Darwin:arm64": "aarch64-apple-darwin",
}
VERSION = re.compile(r"(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\Z")


@dataclass(frozen=True)
class Artifact:
    name: str
    size: int
    sha256: str

    @classmethod
    def read(cls, path):
        digest = hashlib.sha256()
        size = 0
        with path.open("rb") as source:
            while chunk := source.read(1024 * 1024):
                size += len(chunk)
                if size > 256 * 1024 * 1024:
                    raise ValueError("Native installer exceeds 256 MiB")
                digest.update(chunk)
        if not size:
            raise ValueError("Native installer is empty")
        return cls(path.name, size, digest.hexdigest())


POSIX = r'''#!/bin/sh
set -u
umask 077
LC_ALL=C
export LC_ALL
version=@VERSION@
base="https://github.com/neohaskell/NeoHaskell/releases/download/neo-v$version"
@MESSAGES@
fail() {
    printf '%s\n' "$1" >&2
    exit 1
}
for tool in curl uname mktemp chmod rm rmdir wc tr cmp cat; do
    command -v "$tool" >/dev/null 2>&1 || fail "$bootstrap_tools"
done
if command -v sha256sum >/dev/null 2>&1; then
    hash_file() { sha256sum < "$1"; }
elif command -v shasum >/dev/null 2>&1; then
    hash_file() { shasum -a 256 < "$1"; }
else
    fail "$bootstrap_tools"
fi
case "$(uname -s):$(uname -m)" in
@TARGETS@
    *) fail "$bootstrap_platform" ;;
esac
stage=$(mktemp -d "${TMPDIR:-/tmp}/neohaskell-bootstrap.XXXXXXXXXX") || fail "$bootstrap_staging"
cleanup() {
    result=$?
    trap - 0 HUP INT TERM
    rm -f "$stage/candidate.locator" "$stage/locator.expected" "$stage/neo-installer"
    rmdir "$stage"
    exit "$result"
}
trap cleanup 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
fetch() (
    limit=$3
    ulimit -f $(( (limit + 511) / 512 )) || exit 1
    curl -q --fail --silent --show-error --location \
        --proto '=https' --proto-redir '=https' --max-redirs 5 \
        --connect-timeout 15 --max-time 120 --retry 2 --retry-delay 1 \
        --retry-max-time 250 --max-filesize "$limit" \
        --output "$2" "$1"
)
printf '%s\n' "$bootstrap_preparing" >&2
if [ "${1-}" = --candidate-locator ]; then
    [ "$#" -ge 2 ] || fail "$bootstrap_locator"
    locator=$2
    shift 2
else
    fetch "$base/candidate.locator" "$stage/candidate.locator" 128 || fail "$bootstrap_download"
    [ "$(wc -c < "$stage/candidate.locator" | tr -d ' ')" -le 128 ] || fail "$bootstrap_locator"
    locator=$(cat "$stage/candidate.locator") || fail "$bootstrap_locator"
    printf '%s\n' "$locator" > "$stage/locator.expected" || fail "$bootstrap_staging"
    cmp -s "$stage/candidate.locator" "$stage/locator.expected" || fail "$bootstrap_locator"
fi
case "$locator" in
    "$version"@*) candidate_sha=${locator#*@} ;;
    *) fail "$bootstrap_locator" ;;
esac
[ "${#candidate_sha}" -eq 64 ] || fail "$bootstrap_locator"
case "$candidate_sha" in
    *[!0-9a-f]*) fail "$bootstrap_locator" ;;
esac
fetch "$base/$asset" "$stage/neo-installer" "$size" || fail "$bootstrap_download"
[ "$(wc -c < "$stage/neo-installer" | tr -d ' ')" = "$size" ] || fail "$bootstrap_integrity"
actual=$(hash_file "$stage/neo-installer") || fail "$bootstrap_integrity"
actual=${actual%% *}
[ "$actual" = "$sha256" ] || fail "$bootstrap_integrity"
chmod 700 "$stage/neo-installer" || fail "$bootstrap_staging"
if [ -t 1 ] && ( : < /dev/tty ) 2>/dev/null; then
    "$stage/neo-installer" install --candidate-locator "$locator" "$@" < /dev/tty
else
    "$stage/neo-installer" install --candidate-locator "$locator" "$@"
fi
exit "$?"
'''


def generate_posix(version, artifacts, messages):
    rows = []
    for platform, target in TARGETS.items():
        artifact = artifacts[target]
        rows.append(f"    {platform}) asset={artifact.name}; size={artifact.size}; sha256={artifact.sha256} ;;")
    assignments = "\n".join(
        f"{key}={shlex.quote(value)}" for key, value in sorted(messages.items())
        if key.startswith("bootstrap_") and isinstance(value, str)
    )
    return (POSIX.replace("@VERSION@", shlex.quote(version))
            .replace("@MESSAGES@", assignments)
            .replace("@TARGETS@", "\n".join(rows))).encode("utf-8")


POWERSHELL = r'''[CmdletBinding()]
param(
    [string] $Distribution,
    [string] $CandidateLocator,
    [switch] $Yes,
    [string] $Version,
    [switch] $Plain,
    [switch] $Still
)
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$English = @@ENGLISH_POWERSHELL@@
Set-Variable -Name Release -Value '@@RELEASE_VERSION@@' -Option Constant
Set-Variable -Name PosixSize -Value @@POSIX_SIZE@@ -Option Constant
Set-Variable -Name PosixSha256 -Value '@@POSIX_SHA256@@' -Option Constant
$ReleaseRoot = 'https://github.com/neohaskell/NeoHaskell/releases/download/neo-v' + $Release + '/'


function Stop-Bootstrap([string] $Key) {
    throw [InvalidOperationException]::new($Key)
}

function Quote-WindowsArgument([string] $Value) {
    return '"' + [regex]::Replace([regex]::Replace($Value, '(\\*)"', '$1$1\"'), '(\\+)$', '$1$1') + '"'
}

function ConvertFrom-WslBytes([byte[]] $Raw) {
    if (($Raw.Length -ge 2 -and $Raw[0] -eq 255 -and $Raw[1] -eq 254) -or $Raw -contains 0) {
        if ($Raw.Length % 2 -ne 0) { Stop-Bootstrap 'windows_target_invalid' }
        $Text = (New-Object Text.UnicodeEncoding($false, $false, $true)).GetString($Raw)
    } else {
        $Text = (New-Object Text.UTF8Encoding($false, $true)).GetString($Raw)
    }
    $Text = $Text.TrimStart([char] 0xfeff)
    if ($Text.Contains([char] 0)) { Stop-Bootstrap 'windows_target_invalid' }
    return $Text
}

function New-WslStartInfo([string[]] $Arguments, [bool] $Capture) {
    $Info = New-Object System.Diagnostics.ProcessStartInfo
    $Info.FileName = $Wsl
    $Info.UseShellExecute = $false
    $Info.Arguments = (($Arguments | ForEach-Object { Quote-WindowsArgument $_ }) -join ' ')
    if (($Info.Arguments.Length + $Wsl.Length + 4) -gt 32000) { Stop-Bootstrap 'windows_argument_invalid' }
    $Info.RedirectStandardInput = $false
    $Info.RedirectStandardOutput = $Capture
    $Info.RedirectStandardError = $Capture
    return $Info
}

function Invoke-Wsl([string[]] $Arguments, [bool] $Capture) {
    $Info = New-WslStartInfo $Arguments $Capture
    $Process = New-Object System.Diagnostics.Process
    $Process.StartInfo = $Info
    $Started = $false
    try {
        try { $Started = $Process.Start() }
        catch { Stop-Bootstrap 'windows_start_failed' }
        if (-not $Started) { Stop-Bootstrap 'windows_start_failed' }
        if (-not $Capture) {
            $Process.WaitForExit()
            return $Process.ExitCode
        }
        $Streams = @($Process.StandardOutput.BaseStream, $Process.StandardError.BaseStream)
        $Buffers = @((New-Object byte[] 4096), (New-Object byte[] 4096))
        $Saved = @((New-Object IO.MemoryStream), (New-Object IO.MemoryStream))
        $Pending = @($Streams[0].ReadAsync($Buffers[0], 0, 4096), $Streams[1].ReadAsync($Buffers[1], 0, 4096))
        $Done = @($false, $false)
        $Clock = [Diagnostics.Stopwatch]::StartNew()
        try {
            while (-not ($Done[0] -and $Done[1] -and $Process.HasExited)) {
                if ($Clock.Elapsed.TotalSeconds -gt 30) { Stop-Bootstrap 'windows_probe_limit' }
                for ($Index = 0; $Index -lt 2; $Index++) {
                    if (-not $Done[$Index] -and $Pending[$Index].IsCompleted) {
                        $Count = $Pending[$Index].GetAwaiter().GetResult()
                        if ($Count -eq 0) { $Done[$Index] = $true; continue }
                        if ($Saved[$Index].Length + $Count -gt 65536) { Stop-Bootstrap 'windows_probe_limit' }
                        $Saved[$Index].Write($Buffers[$Index], 0, $Count)
                        $Pending[$Index] = $Streams[$Index].ReadAsync($Buffers[$Index], 0, 4096)
                    }
                }
                [Threading.Thread]::Sleep(10)
            }
            if ($Process.ExitCode -ne 0) { Stop-Bootstrap 'windows_dispatch_failed' }
            return ConvertFrom-WslBytes $Saved[0].ToArray()
        } finally {
            foreach ($Memory in $Saved) { $Memory.Dispose() }
        }
    } finally {
        if ($Started -and -not $Process.HasExited) { $Process.Kill() }
        $Process.Dispose()
    }
}

function Read-DistributionRecords([string] $NamesText, [string] $RowsText) {
    $Names = @($NamesText.TrimEnd("`r", "`n") -split '\r?\n')
    $Records = New-Object 'Collections.Generic.Dictionary[string,object]' ([StringComparer]::Ordinal)
    foreach ($Name in $Names) {
        if ($Name -notmatch '^[^\x00-\x1f\x7f]+$') { Stop-Bootstrap 'windows_target_invalid' }
        if ($Records.ContainsKey($Name)) { Stop-Bootstrap 'windows_target_invalid' }
        $Records.Add($Name, $null)
    }
    foreach ($Row in @($RowsText.TrimEnd("`r", "`n") -split '\r?\n')) {
        $Candidates = @()
        foreach ($Name in $Records.Keys) {
            $Pattern = '^\s*(?<default>\*)?\s*' + [regex]::Escape($Name) + '\s+\S.*?\s+(?<version>[0-9]+)\s*$'
            $Match = [regex]::Match($Row, $Pattern)
            if ($Match.Success) {
                $Candidates += @{ Name = $Name; Version = $Match.Groups['version'].Value; Default = $Match.Groups['default'].Value -eq '*' }
            }
        }
        if ($Candidates.Count -gt 1) { Stop-Bootstrap 'windows_target_invalid' }
        if ($Candidates.Count -eq 1) {
            $Record = $Candidates[0]
            if ($null -ne $Records[$Record.Name] -or $Record.Version -cnotmatch '^[12]$') { Stop-Bootstrap 'windows_target_invalid' }
            $Records[$Record.Name] = $Record
        }
    }
    if ($Records.Count -eq 0 -or @($Records.Values | Where-Object { $null -eq $_ }).Count -ne 0) { Stop-Bootstrap 'windows_target_invalid' }
    if (@($Records.Values | Where-Object { $_.Default }).Count -ne 1) { Stop-Bootstrap 'windows_target_invalid' }
    return ,$Records
}

function Read-InstallArguments([string[]] $Values) {
    $VersionSeen = $false
    $DisplaySeen = $false
    for ($Index = 0; $Index -lt $Values.Length; $Index++) {
        $Value = $Values[$Index]
        if ($Value -ceq '--version' -and -not $VersionSeen) {
            $Index++
            if ($Index -ge $Values.Length -or $Values[$Index].Length -gt 32 -or $Values[$Index] -cnotmatch '\A(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\z') { Stop-Bootstrap 'windows_argument_invalid' }
            $VersionSeen = $true
        } elseif (($Value -ceq '--plain' -or $Value -ceq '--still') -and -not $DisplaySeen) {
            $DisplaySeen = $true
        } else { Stop-Bootstrap 'windows_argument_invalid' }
    }
    return ,$Values
}

function Read-BoundedHttps([string] $Address, [int] $Limit) {
    for ($Attempt = 0; $Attempt -lt 3; $Attempt++) {
        $Current = [Uri] $Address
        try {
            for ($Redirect = 0; $Redirect -le 5; $Redirect++) {
                if ($Current.Scheme -cne 'https' -or $Current.UserInfo -ne '') {
                    Stop-Bootstrap 'windows_download_failed'
                }
                $Request = [Net.HttpWebRequest]::Create($Current)
                $Request.AllowAutoRedirect = $false
                $Request.Timeout = 30000
                $Request.ReadWriteTimeout = 30000
                $Request.AutomaticDecompression = [Net.DecompressionMethods]::None
                $Response = $Request.GetResponse()
                try {
                    $Status = [int] $Response.StatusCode
                    if ($Status -in @(301, 302, 303, 307, 308)) {
                        if ($Redirect -eq 5 -or -not $Response.Headers['Location']) {
                            Stop-Bootstrap 'windows_download_failed'
                        }
                        $Current = New-Object Uri($Current, $Response.Headers['Location'])
                        continue
                    }
                    if ($Status -ne 200 -or $Response.ContentLength -gt $Limit) {
                        Stop-Bootstrap 'windows_download_failed'
                    }
                    $Stream = $Response.GetResponseStream()
                    $Buffer = New-Object byte[] 8192
                    $Bytes = New-Object IO.MemoryStream
                    $Deadline = [DateTime]::UtcNow.AddSeconds(60)
                    try {
                        while (($Count = $Stream.Read($Buffer, 0, $Buffer.Length)) -gt 0) {
                            if ($Bytes.Length + $Count -gt $Limit -or [DateTime]::UtcNow -gt $Deadline) {
                                Stop-Bootstrap 'windows_download_failed'
                            }
                            $Bytes.Write($Buffer, 0, $Count)
                        }
                        return ,$Bytes.ToArray()
                    } finally {
                        $Bytes.Dispose()
                        $Stream.Dispose()
                    }
                } finally { $Response.Close() }
            }
        } catch [Net.WebException] {
            $Failure = $_.Exception
            $Retry = $Failure.Status -in @([Net.WebExceptionStatus]::Timeout, [Net.WebExceptionStatus]::ConnectFailure, [Net.WebExceptionStatus]::ConnectionClosed, [Net.WebExceptionStatus]::ReceiveFailure)
            if ($Failure.Response) {
                $Retry = [int] $Failure.Response.StatusCode -in @(408, 429, 500, 502, 503, 504)
                $Failure.Response.Close()
            }
            if (-not $Retry -or $Attempt -eq 2) { Stop-Bootstrap 'windows_download_failed' }
            Start-Sleep -Seconds ([int] [Math]::Pow(2, $Attempt))
        }
    }
    Stop-Bootstrap 'windows_download_failed'
}

try {
    $InstallArguments = @()
    if ($Version) { $InstallArguments += @('--version', $Version) }
    if ($Plain) { $InstallArguments += '--plain' }
    if ($Still) { $InstallArguments += '--still' }
    $Options = Read-InstallArguments $InstallArguments
    if ($CandidateLocator -and $CandidateLocator -cnotmatch ('\A' + [regex]::Escape($Release) + '@[0-9a-f]{64}\z')) { Stop-Bootstrap 'windows_locator_invalid' }
    if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) { Stop-Bootstrap 'windows_host_invalid' }
    $Wsl = Join-Path ([Environment]::GetFolderPath('Windows')) 'System32\wsl.exe'
    if ([Environment]::Is64BitOperatingSystem -and -not [Environment]::Is64BitProcess) {
        $Wsl = Join-Path ([Environment]::GetFolderPath('Windows')) 'Sysnative\wsl.exe'
    }
    if (-not [Environment]::Is64BitOperatingSystem) { Stop-Bootstrap 'windows_host_invalid' }
    if (-not (Test-Path -LiteralPath $Wsl -PathType Leaf)) { Stop-Bootstrap 'windows_wsl_missing' }
    $Interactive = [Environment]::UserInteractive -and -not [Console]::IsInputRedirected -and -not [Console]::IsOutputRedirected
    if (-not $Interactive -and -not $Yes) { Stop-Bootstrap 'windows_consent_required' }
    $NamesText = Invoke-Wsl @('--list', '--quiet') $true
    $RowsText = Invoke-Wsl @('--list', '--verbose') $true
    $Records = Read-DistributionRecords $NamesText $RowsText
    $Selected = $Distribution
    if (-not $Selected) { $Selected = @($Records.Values | Where-Object { $_.Default })[0].Name }
    if (-not $Records.ContainsKey($Selected) -or $Records[$Selected].Version -cne '2') { Stop-Bootstrap 'windows_target_invalid' }
    $Probe = 'printf "%s\n" "$WSL_DISTRO_NAME"; uname -m; cat /proc/1/comm; id -u; if test -w "$HOME" && command -v sudo >/dev/null && command -v base64 >/dev/null; then printf "ready\n"; else printf "permissions\n"; fi; if sudo -n -l -- /bin/sh >/dev/null 2>&1; then printf "sudo-ready\n"; else printf "sudo-interactive\n"; fi'
    $Result = Invoke-Wsl @('--distribution', $Selected, '--exec', '/bin/sh', '-c', $Probe) $true
    $Fields = @($Result.TrimEnd("`r", "`n") -split '\r?\n')
    if ($Fields.Count -ne 6 -or $Fields[0] -cne $Selected -or $Fields[1] -cnotin @('x86_64', 'aarch64') -or $Fields[2] -cne 'systemd' -or $Fields[3] -cnotmatch '^[1-9][0-9]*$') { Stop-Bootstrap 'windows_target_invalid' }
    if ($Fields[4] -cne 'ready' -or (-not $Interactive -and $Fields[5] -cne 'sudo-ready')) { Stop-Bootstrap 'windows_permissions' }
    Write-Host ($English.windows_selected -f $Selected)
    $PreviousProtocol = [Net.ServicePointManager]::SecurityProtocol
    try {
        [Net.ServicePointManager]::SecurityProtocol = $PreviousProtocol -bor [Net.SecurityProtocolType]::Tls12
        $ScriptBytes = Read-BoundedHttps ($ReleaseRoot + 'install') $PosixSize
    } finally { [Net.ServicePointManager]::SecurityProtocol = $PreviousProtocol }
    $Hasher = [Security.Cryptography.SHA256]::Create()
    try { $Digest = ([BitConverter]::ToString($Hasher.ComputeHash($ScriptBytes))).Replace('-', '').ToLowerInvariant() }
    finally { $Hasher.Dispose() }
    if ($ScriptBytes.Length -ne $PosixSize -or $Digest -cne $PosixSha256) { Stop-Bootstrap 'windows_integrity_failed' }
    $Arguments = @()
    if ($CandidateLocator) { $Arguments += @('--candidate-locator', $CandidateLocator) }
    if ($Yes) { $Arguments += '--yes' }
    $Arguments += $Options
    $Decoder = 'payload=$1; shift; script=$(printf "%s" "$payload" | base64 -d) || exit 1; exec /bin/sh -c "$script" neo-bootstrap "$@"'
    $Encoded = [Convert]::ToBase64String($ScriptBytes)
    $Status = Invoke-Wsl (@('--distribution', $Selected, '--exec', '/bin/sh', '-c', $Decoder, 'neo-bootstrap', $Encoded) + $Arguments) $false
    if ($Status -ne 0) {
        [Console]::Error.WriteLine($English.windows_dispatch_failed)
        exit $Status
    }
    Write-Host ($English.windows_completed -f $Selected)
} catch {
    $Key = $_.Exception.Message
    if (-not $English.ContainsKey($Key)) { $Key = 'windows_failure' }
    [Console]::Error.WriteLine(('[' + $Key + '] ' + $English[$Key]))
    exit 1
}
'''


def generate_powershell(version, payload, messages):
    if not VERSION.fullmatch(version) or len(version) > 32:
        raise ValueError("Invalid release version")
    if not payload or len(payload) > 18000:
        raise ValueError("POSIX bootstrap exceeds the bounded Windows argv transport")
    assignments = "\n".join(
        "    '" + key + "' = '" + value.replace("'", "''") + "'"
        for key, value in sorted(messages.items())
        if key.startswith("windows_") and isinstance(value, str)
    )
    return (POWERSHELL.replace("@@ENGLISH_POWERSHELL@@", "@{\n" + assignments + "\n}")
            .replace("@@RELEASE_VERSION@@", version)
            .replace("@@POSIX_SIZE@@", str(len(payload)))
            .replace("@@POSIX_SHA256@@", hashlib.sha256(payload).hexdigest())).encode("utf-8-sig")


def candidate_locator(path, version):
    with path.open("rb") as source:
        raw = source.read(2 * 1024 * 1024 + 1)
    if len(raw) > 2 * 1024 * 1024:
        raise ValueError("Candidate exceeds 2 MiB")
    value = json.loads(raw)
    canonical = json.dumps(value, sort_keys=True, ensure_ascii=False,
                           separators=(",", ":"), allow_nan=False).encode("utf-8")
    if not isinstance(value, dict) or raw != canonical or value.get("version") != version:
        raise ValueError("Candidate must be canonical JSON with the requested version")
    return f"{version}@{hashlib.sha256(raw).hexdigest()}\n".encode("ascii")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--version", required=True)
    parser.add_argument("--artifacts-dir", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--candidate-json", type=Path)
    args = parser.parse_args()
    if not VERSION.fullmatch(args.version) or len(args.version) > 32:
        parser.error("version must be a stable X.Y.Z version of at most 32 characters")
    if args.candidate_json:
        payload = candidate_locator(args.candidate_json, args.version)
        args.output_dir.mkdir(parents=True, exist_ok=True)
        (args.output_dir / "candidate.locator").write_bytes(payload)
        return
    if args.artifacts_dir is None:
        parser.error("--artifacts-dir is required when generating bootstraps")
    artifacts = {target: Artifact.read(args.artifacts_dir / f"neo-installer-{target}")
                 for target in TARGETS.values()}
    messages = json.loads((Path(__file__).resolve().parents[1] / "src/english.json").read_text())
    payload = generate_posix(args.version, artifacts, messages)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "install").write_bytes(payload)
    (args.output_dir / "install").chmod(0o755)
    (args.output_dir / "install.ps1").write_bytes(generate_powershell(args.version, payload, messages))


if __name__ == "__main__":
    main()
