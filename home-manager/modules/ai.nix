{
  pkgs,
  self,
  inputs,
  ...
}:
{
  home.packages = [
    self.packages.${pkgs.system}.claude-code
    self.packages.${pkgs.system}.pexpect-mcp
    self.packages.${pkgs.system}.claude-md
    inputs.nix-ai-tools.packages.${pkgs.system}.opencode
    inputs.nix-ai-tools.packages.${pkgs.system}.coderabbit-cli
    inputs.nix-ai-tools.packages.${pkgs.system}.codex
    pkgs.pueue
  ];
}
