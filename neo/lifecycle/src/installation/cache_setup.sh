PATH=/usr/bin:/bin
export PATH
umask 077
target=$1
main=/etc/nix/nix.conf
pending=/etc/nix/.neohaskell-cache-setup
case "$target" in /etc/nix/nix.conf|/etc/nix/nix.custom.conf) ;; *) exit 1 ;; esac
hash() {
    if [ -x /usr/bin/sha256sum ]; then sha256sum < "$1" | cut -d ' ' -f 1
    else shasum -a 256 < "$1" | cut -d ' ' -f 1
    fi
}
matches() {
    [ ! -L "$main" ] && [ ! -L "$target" ] && [ "$(hash "$main")" = "$2" ] && [ "$(hash "$target")" = "$3" ]
}
# A reachable client may be talking to a daemon with a different configuration.
case "$(uname -s)" in
    Linux)
        service=nix-daemon.service
        socket=nix-daemon.socket
        pid=$(systemctl show --property=MainPID --value "$service")
        [ "$pid" -gt 1 ]
        executable=$(readlink "/proc/$pid/exe")
        case "$executable" in /nix/store/*/bin/nix-daemon|/nix/store/*/bin/nix) ;; *) exit 1 ;; esac
        units=${executable%/bin/*}/lib/systemd/system
        for unit in "$service" "$socket"; do
            systemctl is-active --quiet "$unit"
            [ "$(systemctl show --property=NeedDaemonReload --value "$unit")" = no ]
            [ -z "$(systemctl show --property=DropInPaths --value "$unit")" ]
            cmp -s "$units/$unit" "$(systemctl show --property=FragmentPath --value "$unit")"
        done
        [ "$target" -ef "/proc/$pid/root$target" ]
        tr '\000' '\n' < "/proc/$pid/environ" > /dev/null
        if tr '\000' '\n' < "/proc/$pid/environ" | grep -Eq '^(NIX_CONF_DIR|NIX_CONFIG|NIX_USER_CONF_FILES|NIX_CONFIG_HOME|XDG_CONFIG_HOME|XDG_CONFIG_DIRS)='; then exit 1; fi
        ;;
    Darwin)
        label=org.nixos.nix-daemon
        program=/nix/var/nix/profiles/default/bin/nix-daemon
        if [ "$target" = /etc/nix/nix.custom.conf ]; then
            label=systems.determinate.nix-daemon
            program=/usr/local/bin/determinate-nixd
        fi
        job=$(/bin/launchctl print "system/$label")
        printf '%s\n' "$job" | grep -Fqx "$(printf '\tprogram = %s' "$program")"
        printf '%s\n' "$job" | grep -Fqx "$(printf '\tstate = running')"
        if printf '%s\n' "$job" | grep -Eq '(NIX_CONF_DIR|NIX_CONFIG|NIX_USER_CONF_FILES|XDG_CONFIG_HOME|XDG_CONFIG_DIRS) =>|^[[:space:]]+--(option|store|config)'; then exit 1; fi
        ;;
    *) exit 1 ;;
esac
mkdir "$pending"
trap 'rm -f "$pending/config"; rmdir "$pending"' EXIT
matches "$@"
cp -p "$target" "$pending/config"
printf '\n%s\n' "$4" >> "$pending/config"
matches "$@"
trap - EXIT
mv "$pending/config" "$target"
case "$(uname -s)" in
    Linux) /usr/bin/systemctl restart nix-daemon.service ;;
    Darwin)
        /bin/launchctl kickstart -k "system/$label" ;;
    *) exit 1 ;;
esac
rmdir "$pending"
