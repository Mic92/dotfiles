# Configure openwrt with nix

This module uses nixos modules to configure the openwrt uci configuration. The
resulting configuration is converted to a format that the `uci batch` command
understands
(https://openwrt.org/docs/guide-user/base-system/uci#uci_dataobject_model). It
also supports the use of secrets, which are stored in an encrypted
[sops](https://github.com/mozilla/sops) file. Secrets are interpolated with the
following syntax: `@secret_key_from_sops@`.

# Example:

See [example.nix](example.nix), [flake.nix](flake.nix) and [Justfile](Justfile)
on how to use this project. Tipp: The justfile is read by this
[tool](https://github.com/casey/just)

## State of this project

- Works for me™
- I use this code for my own router. I do not intend to support features that go
  beyond what I personally need. If you take up the idea and create a real
  project, let me know and I might switch to your project and contribute to your
  project.

## Related work

- https://gitea.c3d2.de/zentralwerk/network/src/branch/master/nix/pkgs/ap.nix#L142
