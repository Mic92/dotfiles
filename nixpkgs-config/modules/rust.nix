{ pkgs, ... }:
{
  home.packages = with pkgs; [
    rustfmt
    clippy
    rls
    (writeScriptBin "rust-doc" ''
      #! ${stdenv.shell} -e
      exec "${firefox}" "${rustc.doc}/share/doc/rust/html/index.html"
    '')
  ];
}
