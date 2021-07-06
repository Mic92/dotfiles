{ pkgs, ... }:
{
  programs.rbw.enable = true;
  programs.rbw.settings.base_url = "https://bitwarden.thalheim.io";
  programs.rbw.settings.email = "joerg@higgsboson.tk";
  programs.rbw.settings.pinentry = "qt";
}
