{ pkgs, ... }:
{
  home.packages = with pkgs; [
    rustfmt
    (writeScriptBin "rust-doc" ''
       #! ${stdenv.shell} -e
       exec "${firefox}" "${rustc.doc}/share/doc/rust/html/index.html"
    '')
  ];
}
