# Configure openwrt with nix
This module uses nixos modules to configure the openwrt uci configuration. The
resulting configuration is converted to a format that the `uci batch` command
understands
(https://openwrt.org/docs/guide-user/base-system/uci#uci_dataobject_model).  It
also supports the use of secrets, which are stored in an encrypted
[sops](https://github.com/mozilla/sops) file. Secrets are interpolated with the
following syntax: `@secret_key_from_sops@`.

# Example:

See [example.nix](example.nix), [flake.nix](flake.nix) and [Justfile](Justfile) on how to use this project.
Tipp: The justfile is read by this [tool](https://github.com/casey/just)

## State of this project

- Works for me™
- I use this code for my own router. I do not intend to support features that go
  beyond what I personally need. If you take up the idea and create a real
  project, let me know and I might switch to your project and contribute to your
  project.

## Known bugs
- It is not possible to reliably delete all elements of a list section with uci
  batch command. Currently, the code attempts to delete up to 10 list sections
  and then create a new list:
  https://github.com/Mic92/dotfiles/blob/00e178a0953461f676ecc9c0ab5b36a0742e12bd/openwrt/nix_uci/__init__.py#L108


## Related work
- https://gitea.c3d2.de/zentralwerk/network/src/branch/master/nix/pkgs/ap.nix#L142
