# Configure openwrt with nix
This module uses nixos modules to configure. The resulting configuration is
seralized into a format understood by `uci batch` command
(https://openwrt.org/docs/guide-user/base-system/uci#uci_dataobject_model).
Furthermore it supports to use secrets stored in a encrypted
[sops](https://github.com/mozilla/sops) file. Secrets are interpolate using the
following syntax `@secret_key_from_sops@` 

# Example:

See [example.nix](example.nix), [flake.nix](flake.nix) and [apply.sh](apply.sh) on how to use this project.

## State of this project
- Works for me (TM)
- I use this code for my own router. I do not plan to support features beyond
  what I personally need. If you indend to pick up the idea and create a proper
  project based on it, let me know and I might switch and contribute to your
  project instead.

## Known bugs
- It's not possible to reliably delete all element of a list section with uci
  batch command. Currently, the code will try to delete up to 10 list sections
  and then create a new list
  (https://github.com/Mic92/dotfiles/blob/00e178a0953461f676ecc9c0ab5b36a0742e12bd/openwrt/nix_uci/__init__.py#L108.).
