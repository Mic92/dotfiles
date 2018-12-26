{ pkgs, ...}:

{
  home.packages = [ pkgs.myvim ];
  python.packages = [
    "pyls-mypy"
    "pyls-isort"
    "pyls-black"
  ];
}
