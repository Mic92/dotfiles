{ pkgs
, ...
}: {
  home.packages = with pkgs; [ radare2 ];
}
