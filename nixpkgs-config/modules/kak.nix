{ pkgs, ... }:
let
  config = pkgs.writeTextFile (rec {
    name = "kakrc.kak";
    destination = "/share/kak/autoload/${name}";
    text = ''
      echo -to-file /tmp/kak foo
      eval %sh{kak-lsp --kakoune -s $kak_session}
      lsp-enable
    '';
  });
  myKakoune = pkgs.kakoune.override {
    plugins = with pkgs.kakounePlugins; [
      config
      parinfer-rust
      kak-lsp
    ];
  };
in
{
  programs.kakoune = {
    enable = true;
    plugins = with pkgs.kakounePlugins; [
      kak-fzf
      kak-lsp
      powerline-kak
      kakboard
    ];
    extraConfig = ''
      decl str grepcmd 'rg --column'
      eval %sh{kak-lsp --kakoune -s $kak_session}
      lsp-enable
      map global user l %{: enter-user-mode lsp<ret>} -docstring "LSP mode"
    '';
  };
}
