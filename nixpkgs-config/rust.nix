{ pkgs, ... }:
{
  home.packages = with pkgs; [
    rustc
    cargo
    rustfmt
    rustracer
    (writeScriptBin "rust-doc" ''
       #! ${stdenv.shell} -e
       exec "${firefox}" "${rustc.doc}/share/doc/rust/html/index.html"
    '')
  ];
}
