{ pkgs
, config
, ...
}: {
  home.packages = with pkgs;
    [
      pwndbg
      radare2
    ]
    ++ (with config.nur.repos.mic92; [
      gdbgui
      gdb-dashboard
    ]);
}
