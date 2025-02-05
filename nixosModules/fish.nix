{ pkgs, lib, ... }:
{
  programs.fish.enable = true;
  # shadows better builtin completions
  environment.etc."fish/generated_completions".source = lib.mkForce (
    pkgs.writeText "fish-completions" ''
      mkdir $out
    ''
  );
}
