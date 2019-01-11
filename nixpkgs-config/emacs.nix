{ pkgs, ...}:
{
  programs.emacs.package = pkgs.emacs;
  programs.emacs.enable = true;

  home.packages = with pkgs; [
    gocode
    godef
    gocode
    go-tools
    gogetdoc
    impl
    gometalinter
  ];
}
