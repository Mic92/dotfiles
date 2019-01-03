{ pkgs, ...}:
{
  home.packages = with pkgs; [
    emacs
    gocode
    godef
    gocode
    go-tools
    gogetdoc
    impl
    gometalinter
  ];
  python.packages = [
    "pyls-mypy"
    "pyls-isort"
    "pyls-black"
  ];
}
