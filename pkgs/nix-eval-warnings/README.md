# nix-eval-warnings

Extract evaluation warnings from Nix flakes using nix-eval-jobs.

## Usage

```bash
nix-eval-warnings .#checks
nix-eval-warnings .#packages.x86_64-linux
nix-eval-warnings --json .#packages
```

## Exit Codes

| Code | Meaning           |
| ---- | ----------------- |
| 0    | No warnings found |
| 1    | Warnings found    |
| 2    | Error             |

## Output

```
Found 3 warning(s):

## 'system' has been renamed to/replaced by 'stdenv.hostPlatform.system'

  - x86_64-linux.nixos-eva
    nixosModules/phantun
    option: systemd.services.phantun.serviceConfig

## This package is deprecated

  - x86_64-linux.bar
    myinput/packages.nix:15
```

For `builtins.warn` calls, the source includes line numbers. For NixOS module
warnings, the option path is shown to help locate the issue.

Store paths are resolved to flake input names. Progress is shown when running in
a terminal.

## JSON Output

```json
[
  {
    "attr": "x86_64-linux.nixos-eva",
    "warning_type": "'system' has been renamed to/replaced by 'stdenv.hostPlatform.system'",
    "source": "nixosModules/phantun",
    "option_path": "systemd.services.phantun.serviceConfig"
  }
]
```

## Limitations

- Only the first warning per attribute is captured
- NixOS module warnings don't include line numbers (Nix limitation)

## Options

```
--json   Output as JSON
```
