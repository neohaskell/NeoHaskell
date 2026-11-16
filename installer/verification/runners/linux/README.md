# Disposable Linux systemd runner

Run both architecture lanes from the repository root:

```sh
python3 installer/verification/runners/linux/run.py
```

Run one lane with `--architecture aarch64` or `--architecture x86_64`. The Docker engine must target Linux and support the requested image platform. Docker builds need network access to pull `ubuntu:24.04` and its packages.

The runner builds run-specific `neo-installer-check-*` images and creates uniquely named containers. Each container runs privileged with a private cgroup namespace, no network, and no host data mounts. It removes only the container ID that it created. It leaves the uniquely tagged images in Docker.

Each lane checks the image architecture, systemd as PID 1, an active `default.target`, and working `systemctl` and D-Bus sockets. It then starts a transient systemd service as `jess`. It records the source revision, runner file hashes, image ID, Docker engine identity, probe output, and cleanup result under `.agents-state/installer-implementation/linux-runner/`.

This runner proves only that the selected Docker engine can host this systemd probe. It does not test installer artifacts, Nix, a standard Nix daemon, a product installation, or operating system compatibility. A passing container is not product matrix evidence or a support floor.
