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
diag_state=started
diag_step=starting
diag_locator_attempts=0
diag_installer_attempts=0
diag_locator_ms=0
diag_installer_ms=0
diag_error=none
diag_platform=unknown
diag_steps='"started"'
diag_change=none
diag_attempts=0
diag_recovery=retry
diag_path=
diag_identity=
diag_record_identity=
diag_created=0
diag_elapsed=0
stage=
diag_stat() {
    if [ "$diag_os" = Darwin ]; then stat -f '%u %Lp %l %z %d:%i' "$1"
    else stat -c '%u %a %h %s %d:%i' "$1"; fi
}
diag_acl() {
    if [ "$diag_os" = Darwin ]; then
        acl=$(ls -ldne "$1") || return 1
        printf '%s\n' "$acl" | awk 'NR > 1 && $0 !~ /^[ \t]*[0-9]+: [^ \t]+ (inherited )?deny / {bad=1} END {exit bad}'
    else
        mark=$(ls -ld "$1") || return 1
        case "${mark%% *}" in
            *+*) return 1 ;;
        esac
    fi
}
diag_directory() {
    [ ! -L "$1" ] && [ -d "$1" ] || return 1
    set -- "$1" $(diag_stat "$1")
    [ "$#" = 6 ] || return 1
    [ "$2" = 0 ] || [ "$2" = "$diag_uid" ] || return 1
    mode=$((0$3))
    [ $((mode & 18)) = 0 ] || { [ "$2" = 0 ] && [ $((mode & 512)) != 0 ]; } || return 1
    diag_acl "$1"
}
diag_walk() (
    path=$1
    create=$2
    case "$path" in /*) ;; *) exit 1 ;; esac
    case "/$path/" in */../*|*/./*) exit 1 ;; esac
    utf8=$(printf '%s' "$path" | iconv -f UTF-8 -t UTF-8) || exit 1
    [ "$utf8" = "$path" ] || exit 1
    [ "$(printf '%s' "$path" | tr -d '\000-\037\177')" = "$path" ] || exit 1
    printf '%s\n' "$path" | awk '/\302[\200-\237]/ {exit 1}' || exit 1
    cd / || exit 1
    diag_directory . || exit 1
    while [ -n "$path" ]; do
        path=${path#/}
        part=${path%%/*}
        if [ "$path" = "$part" ]; then path=; else path=${path#*/}; fi
        [ -n "$part" ] || continue
        part=./$part
        if [ ! -e "$part" ] && [ ! -L "$part" ] && [ "$create" = yes ]; then
            mkdir -m 700 "$part" || exit 1
            if [ "$diag_os" = Darwin ]; then chmod -N "$part" || exit 1; fi
        fi
        diag_directory "$part" || exit 1
        cd -P "$part" || exit 1
    done
    set -- $(diag_stat .)
    [ "$1" = "$diag_uid" ] && [ $((0$2 & 18)) = 0 ] || exit 1
    pwd -P
)
diag_file() {
    [ ! -L "$1" ] && [ -f "$1" ] || return 1
    set -- "$1" $(diag_stat "$1")
    [ "$#" = 6 ] && [ "$2" = "$diag_uid" ] && [ "$3" = 600 ] && [ "$4" = 1 ] || return 1
    diag_acl "$1"
}
diag_now() {
    clock=$(date '+%s %N') || return 1
    set -- $clock
    seconds=${1-}
    case "$seconds" in ''|*[!0-9]*) return 1 ;; esac
    [ "${#seconds}" -le 10 ] || return 1
    nanos=${2-}
    case "$nanos" in ''|*[!0-9]*) nanos=000000000 ;; esac
    [ "${#nanos}" -eq 9 ] || return 1
    printf '%s\n' "$((seconds * 1000 + 1${nanos%??????} - 1000))"
}
diag_transaction() (
    cd -P "$diag_path" || exit 1
    [ "$(diag_walk "$diag_path" no)" = "$diag_path" ] || exit 1
    set -- $(diag_stat .)
    [ "$2" = 700 ] || [ "$2" = 2700 ] || exit 1
    anchor=$5
    [ -z "$diag_identity" ] || [ "$anchor" = "$diag_identity" ] || exit 1
    if [ ! -e lock ] && [ ! -L lock ]; then (set -C; : > lock) || exit 1; fi
    diag_file lock || exit 1
    set -- $(diag_stat lock)
    [ "$4" = 0 ] || exit 1
    lock_identity=$5
    exec 9<>lock || exit 1
    if [ "$diag_os" = Darwin ]; then
        lockf -t 0 9 || exit 1
    else flock -w 0.1 9 || exit 1; fi
    diag_file lock || exit 1
    set -- $(diag_stat lock)
    [ "$4" = 0 ] && [ "$5" = "$lock_identity" ] || exit 1
    [ "$(diag_walk "$diag_path" no)" = "$diag_path" ] || exit 1
    set -- $(diag_stat "$diag_path")
    [ "$5" = "$anchor" ] || exit 1
    count=$(
        { find . ! -name . -prune -exec /bin/sh -c 'printf "%s\n" "$#"' inventory {} + &&
          printf 'complete\n'; } |
        awk '$0=="complete" {complete=1; next} {count+=$0; if(count>10242) exit 1}
             END {if(!complete || count>10242) exit 1; print count+0}'
    ) || exit 1
    case "$count" in ''|*[!0-9]*) exit 1 ;; esac
    inventory=
    total=0
    pending=0
    for name in .pending-v1 v1-*; do
        [ -e "$name" ] || [ -L "$name" ] || continue
        diag_file "$name" || exit 1
        set -- $(diag_stat "$name")
        bytes=$4
        identity=$5
        if [ "$name" = .pending-v1 ]; then
            [ "$bytes" -le 16384 ] || exit 1
            pending=1
            continue
        fi
        parsed=$(printf '%s\n' "$name" | awk '/^v1-[0-9a-f]+-[0-9a-f]+\.json$/ {if(length($0)==57) print}')
        [ "$parsed" = "$name" ] || exit 1
        stamp=${name#v1-}; stamp=${stamp%%-*}
        rid=${name#v1-????????????????-}; rid=${rid%.json}
        [ "${#stamp}" = 16 ] && [ "${#rid}" = 32 ] || exit 1
        case "$stamp" in [89abcdef]*) exit 1 ;; esac
        created=$(printf '%d' "0x$stamp") || exit 1
        [ "$bytes" -ge 1024 ] && [ "$bytes" -le 16384 ] || exit 1
        header=$(printf '{"schema":1,"id":"%s","created_ms":%s,\n' "$rid" "$created")
        observed=$(head -c "$((${#header}+1))" "$name" | od -An -tx1 | tr -d ' \n') || exit 1
        expected=$(printf '%s\n' "$header" | od -An -tx1 | tr -d ' \n') || exit 1
        [ "$observed" = "$expected" ] || exit 1
        inventory="$inventory$name $created $bytes $identity
"
        total=$((total + bytes))
    done
    now=$(diag_now) || exit 1
    if [ "$diag_state" != started ]; then
        diag_file "$diag_name" || exit 1
        set -- $(diag_stat "$diag_name")
        [ "$5" = "$diag_record_identity" ] || exit 1
        [ "$((now - diag_created))" -lt 604800000 ] || exit 1
    else [ ! -e "$diag_name" ] && [ ! -L "$diag_name" ] || exit 1; fi
    removals=
    removed=0
    ordered=$(printf '%s' "$inventory" | sort) || exit 1
    while IFS=' ' read -r name created bytes identity; do
        [ -n "$name" ] || continue
        [ "$name" != "$diag_name" ] || continue
        if [ "$((now - created))" -ge 604800000 ] || [ "$((total + 16384))" -gt 10485760 ]; then
            removals="$removals$name $identity
"
            total=$((total - bytes))
            removed=$((removed + 1))
        fi
    done <<EOF_INVENTORY
$ordered
EOF_INVENTORY
    [ "$((total + 16384))" -le 10485760 ] || exit 1
    extra=1
    [ "$diag_state" != started ] || extra=2
    [ "$((count - pending - removed + extra))" -le 10242 ] || exit 1
    if [ "$pending" = 1 ]; then rm .pending-v1 || exit 1; fi
    while IFS=' ' read -r name identity; do
        [ -n "$name" ] || continue
        diag_file "$name" || exit 1
        set -- $(diag_stat "$name")
        [ "$5" = "$identity" ] || exit 1
        rm "$name" || exit 1
    done <<EOF_REMOVALS
$removals
EOF_REMOVALS
    (set -C; : > .pending-v1) || exit 1
    diag_file .pending-v1 || exit 1
    {
        printf '{"schema":1,"id":"%s","created_ms":%s,\n' "$diag_id" "$diag_created"
        printf '"report":{"protocol":1,"route":"posix","release":"%s","platform":"%s","state":"%s","completed_steps":[%s],"elapsed_ms":%s,"step":"%s","attempts":{"locator":%s,"installer":%s},"timings_ms":{"locator":%s,"installer":%s},"error":"%s","change":"%s","recovery":"%s","exit":%s}}' "$version" "$diag_platform" "$diag_state" "$diag_steps" "$diag_elapsed" "$diag_step" "$diag_locator_attempts" "$diag_installer_attempts" "$diag_locator_ms" "$diag_installer_ms" "$diag_error" "$diag_change" "$diag_recovery" "$diag_exit"
        printf '%1024s\n' ''
    } > .pending-v1 || exit 1
    [ "$(wc -c < .pending-v1)" -le 16384 ] || exit 1
    [ "$(diag_walk "$diag_path" no)" = "$diag_path" ] || exit 1
    set -- $(diag_stat "$diag_path")
    [ "$5" = "$anchor" ] || exit 1
    mv .pending-v1 "$diag_name" || exit 1
    diag_file "$diag_name" || exit 1
    set -- $(diag_stat "$diag_name")
    printf '%s %s\n' "$anchor" "$5"
)
diag_begin() {
    for tool in uname id stat ls awk find sort head date od tr mkdir mv chmod rm wc iconv; do
        command -v "$tool" >/dev/null 2>&1 || return 1
    done
    diag_os=$(uname -s) || return 1
    case "$diag_os" in Darwin) command -v lockf >/dev/null 2>&1 || return 1 ;; Linux) command -v flock >/dev/null 2>&1 || return 1 ;; *) return 1 ;; esac
    diag_uid=$(id -u) || return 1
    diag_walk "${HOME-}" no >/dev/null || return 1
    base_path=${XDG_STATE_HOME:-${HOME-}/.local/state}
    diag_path=$(diag_walk "$base_path/neohaskell-diagnostics" yes) || return 1
    diag_created=$(diag_now) || return 1
    diag_id=$(od -An -N16 -tx1 /dev/urandom | tr -d ' \n') || return 1
    [ "${#diag_id}" = 32 ] || return 1
    case "$diag_id" in *[!0-9a-f]*) return 1 ;; esac
    diag_name=$(printf 'v1-%016x-%s.json' "$diag_created" "$diag_id") || return 1
    diag_exit=null
    saved=$(diag_transaction) || return 1
    diag_identity=${saved%% *}
    diag_record_identity=${saved#* }
}
diag_finish() {
    [ -n "$diag_identity" ] || return 1
    diag_state=finished
    diag_exit=$1
    now=$(diag_now) || return 1
    diag_elapsed=$((now - diag_created))
    [ "$diag_elapsed" -ge 0 ] || diag_elapsed=0
    if [ "$1" = 0 ]; then diag_recovery=none
    elif [ "$diag_error" = none ]; then diag_error=native_failed; fi
    diag_transaction >/dev/null
}

fail() {
    diag_error=$2
    printf '%s\n' "$1" >&2
    exit 1
}
cleanup() {
    result=$?
    trap - 0 HUP INT TERM
    saved=no
    if diag_finish "$result" 2>/dev/null; then saved=yes; fi
    if [ "$result" != 0 ]; then
        if [ "$saved" = yes ]; then
            printf '%s%s\n' "$bootstrap_report_saved" "$diag_path/$diag_name" >&2
        else printf '%s\n' "$bootstrap_report_unavailable" >&2; fi
    fi
    if [ -n "$stage" ]; then
        rm -f "$stage/candidate.locator" "$stage/locator.expected" "$stage/neo-installer"
        rmdir "$stage"
    fi
    exit "$result"
}
trap cleanup 0
trap 'diag_error=interrupted; exit 129' HUP
trap 'diag_error=interrupted; exit 130' INT
trap 'diag_error=interrupted; exit 143' TERM
diag_begin 2>/dev/null || diag_identity=
diag_step=tools_check
for tool in curl uname mktemp chmod rm rmdir wc tr cmp cat sleep; do
    command -v "$tool" >/dev/null 2>&1 || fail "$bootstrap_tools" tools_missing
done
if command -v sha256sum >/dev/null 2>&1; then
    hash_file() { sha256sum < "$1"; }
elif command -v shasum >/dev/null 2>&1; then
    hash_file() { shasum -a 256 < "$1"; }
else
    fail "$bootstrap_tools" tools_missing
fi
diag_steps="$diag_steps,\"tools_checked\""
diag_step=platform_selection
case "$(uname -s):$(uname -m)" in
@TARGETS@
    *) fail "$bootstrap_platform" platform_unsupported ;;
esac
diag_steps="$diag_steps,\"platform_selected\""
diag_step=stage_creation
stage=$(mktemp -d "${TMPDIR:-/tmp}/neohaskell-bootstrap.XXXXXXXXXX") || fail "$bootstrap_staging" staging_failed
diag_steps="$diag_steps,\"stage_created\""
diag_change=private_stage_created
fetch_once() (
    limit=$3
    ulimit -f $(( (limit + 511) / 512 )) || exit 1
    curl -q --fail --silent --show-error --location \
        --proto '=https' --proto-redir '=https' --max-redirs 5 \
        --connect-timeout 15 --max-time 120 --max-filesize "$limit" \
        --write-out '%{http_code}' --output "$2" "$1"
)
fetch() {
    diag_attempts=0
    fetch_started=$(diag_now 2>/dev/null) || fetch_started=0
    fetched=1
    while [ "$diag_attempts" -lt 3 ]; do
        diag_attempts=$((diag_attempts + 1))
        case "$diag_step" in
            locator_download) diag_locator_attempts=$diag_attempts ;;
            installer_download) diag_installer_attempts=$diag_attempts ;;
        esac
        fetch_status=0
        http_status=$(fetch_once "$@") || fetch_status=$?
        if [ "$fetch_status" -eq 0 ]; then fetched=0; break; fi
        case "$fetch_status:$http_status" in
            28:*|22:408|22:429|22:500|22:502|22:503|22:504) ;;
            *) break ;;
        esac
        [ "$diag_attempts" -lt 3 ] && sleep 1 || break
    done
    fetch_finished=$(diag_now 2>/dev/null) || fetch_finished=$fetch_started
    fetch_elapsed=$((fetch_finished - fetch_started))
    [ "$fetch_elapsed" -ge 0 ] || fetch_elapsed=0
    case "$diag_step" in
        locator_download) diag_locator_ms=$fetch_elapsed ;;
        installer_download) diag_installer_ms=$fetch_elapsed ;;
    esac
    return "$fetched"
}
printf '%s\n' "$bootstrap_preparing" >&2
diag_step=locator_validation
if [ "${1-}" = --candidate-locator ]; then
    [ "$#" -ge 2 ] || fail "$bootstrap_locator" locator_invalid
    locator=$2
    shift 2
else
    diag_step=locator_download
    fetch "$base/candidate.locator" "$stage/candidate.locator" 128 || fail "$bootstrap_download" download_failed
    diag_step=locator_validation
    [ "$(wc -c < "$stage/candidate.locator" | tr -d ' ')" -le 128 ] || fail "$bootstrap_locator" locator_invalid
    locator=$(cat "$stage/candidate.locator") || fail "$bootstrap_locator" locator_invalid
    printf '%s\n' "$locator" > "$stage/locator.expected" || fail "$bootstrap_staging" staging_failed
    cmp -s "$stage/candidate.locator" "$stage/locator.expected" || fail "$bootstrap_locator" locator_invalid
fi
case "$locator" in
    "$version"@*) candidate_sha=${locator#*@} ;;
    *) fail "$bootstrap_locator" locator_invalid ;;
esac
[ "${#candidate_sha}" -eq 64 ] || fail "$bootstrap_locator" locator_invalid
case "$candidate_sha" in
    *[!0-9a-f]*) fail "$bootstrap_locator" locator_invalid ;;
esac
diag_steps="$diag_steps,\"locator_verified\""
diag_step=installer_download
fetch "$base/$asset" "$stage/neo-installer" "$size" || fail "$bootstrap_download" download_failed
diag_step=installer_validation
[ "$(wc -c < "$stage/neo-installer" | tr -d ' ')" = "$size" ] || fail "$bootstrap_integrity" integrity_failed
actual=$(hash_file "$stage/neo-installer") || fail "$bootstrap_integrity" integrity_failed
actual=${actual%% *}
[ "$actual" = "$sha256" ] || fail "$bootstrap_integrity" integrity_failed
chmod 700 "$stage/neo-installer" || fail "$bootstrap_staging" staging_failed
diag_steps="$diag_steps,\"installer_verified\",\"native_started\""
diag_change=native_started
diag_step=native
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
        rows.append(f"    {platform}) diag_platform={target}; asset={artifact.name}; size={artifact.size}; sha256={artifact.sha256} ;;")
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
    $Probe = 'printf "%s\n" "$WSL_DISTRO_NAME"; uname -m; cat /proc/1/comm; id -u; if test -w "$HOME" && command -v base64 >/dev/null; then printf "ready\n"; else printf "permissions\n"; fi'
    $Result = Invoke-Wsl @('--distribution', $Selected, '--exec', '/bin/sh', '-c', $Probe) $true
    $Fields = @($Result.TrimEnd("`r", "`n") -split '\r?\n')
    if ($Fields.Count -ne 5 -or $Fields[0] -cne $Selected -or $Fields[1] -cnotin @('x86_64', 'aarch64') -or $Fields[2] -cne 'systemd' -or $Fields[3] -cnotmatch '^[1-9][0-9]*$') { Stop-Bootstrap 'windows_target_invalid' }
    if ($Fields[4] -cne 'ready') { Stop-Bootstrap 'windows_permissions' }
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
