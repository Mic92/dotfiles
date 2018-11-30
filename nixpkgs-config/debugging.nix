{ pkgs, ... }:
{
  home.packages = with pkgs; [
    pwndbg
    radare2
  ] ++ (with nur.repos.mic92; [
    gdbgui
    gdb-dashboard
  ]);
}
