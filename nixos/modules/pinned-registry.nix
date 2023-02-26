{ pkgs, inputs, ... }:
let
  overrides = pkgs.writeText "overrides.json" (builtins.toJSON {
    nixpkgs = inputs.nixpkgs;
    home-manager = inputs.home-manager;
    nur = inputs.nur;
  });
  patch-registry = pkgs.writers.writePython3 "patch-flake-registry.py" {} ''
    import sys
    import json
    overrides = json.load(open(sys.argv[1]))
    registry = json.load(sys.stdin)
    for entry in registry["flakes"]:
        replacement = overrides.get(entry["from"]["id"])
        if replacement:
            entry["to"] = dict(type="path", path=replacement)

    json.dump(registry, sys.stdout)
  '';
in {
  nix.settings.flake-registry = pkgs.runCommand "flake-registry.json" {} ''
    ${patch-registry} ${overrides} < ${inputs.flake-registry}/flake-registry.json > $out
  '';
}
