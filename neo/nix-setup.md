# Configure Nix for NeoHaskell

Set up a working multi-user Nix installation before you run the NeoHaskell
release installer. The NeoHaskell Cachix cache provides the tools. The IOG
cache provides prebuilt dependencies. If you need Nix itself, follow the
[official Nix installation instructions](https://nix.dev/install-nix.html).

On Windows, use a WSL2 distribution with systemd enabled. Follow the Linux
multi-user Nix instructions inside that distribution.
[Enable systemd in WSL2](https://learn.microsoft.com/en-us/windows/wsl/systemd/)

## Add the caches to the daemon configuration

Have the administrator add these settings. Keep existing features, caches, and
keys, including the default Nix cache. Keep signature checking enabled and leave
`trusted-users` at its existing setting. [Nix binary cache guide](https://nix.dev/guides/recipes/add-binary-cache.html)

```ini
extra-experimental-features = nix-command flakes
extra-substituters = https://neohaskell.cachix.org https://cache.iog.io
extra-trusted-public-keys = neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Use the configuration source that manages your daemon.

- **Standard multi-user Nix.** Add the settings to the system
  `/etc/nix/nix.conf`. On WSL2, do this inside the distribution where you’ll
  run NeoHaskell. On systemd, restart the daemon with
  `sudo systemctl restart nix-daemon.service`. On macOS, restart the machine
  to load the new configuration.
- **Determinate Nix.** Add the settings to `/etc/nix/nix.custom.conf`. Restart
  the Determinate daemon using the instructions for your platform. If a
  declarative module manages the file, apply the changes through that module.
  [Determinate configuration](https://docs.determinate.systems/determinate-nix/#configuring-determinate-nix),
  [Determinate daemon restart](https://docs.determinate.systems/guides/migrate-to-flakehub-cache/)
- **NixOS or nix-darwin managing Nix.** Add the lists below to `nix.settings`
  in the system configuration. Apply your changes with your usual `nixos-rebuild switch` or
  `darwin-rebuild switch` command. The Determinate nix-darwin module uses
  `determinateNix.customSettings` in place of `nix.settings`.
  [NixOS cache configuration](https://nix.dev/guides/recipes/add-binary-cache.html#nixos),
  [nix-darwin options](https://nix-darwin.github.io/nix-darwin/manual/),
  [Determinate nix-darwin guide](https://docs.determinate.systems/guides/nix-darwin/)

```nix
nix.settings = {
	extra-experimental-features = [ "nix-command" "flakes" ];
	extra-substituters = [
		"https://neohaskell.cachix.org"
		"https://cache.iog.io"
	];
	extra-trusted-public-keys = [
		"neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="
		"hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
	];
};
```

## Check daemon access from your account

Open a new shell as the user who will run NeoHaskell and run the check without
`sudo`.

```sh
nix store info --store daemon --offline --json
```

If the command reports a daemon connection or permissions error, ask the
administrator to check that the daemon is running and that your account can
connect. A successful response confirms daemon access. The release installation
checks whether the daemon can obtain the selected packages. If the installer
reports a cache or signature error, check both cache URLs and public keys in
the system configuration and confirm that the daemon has loaded the changes.
