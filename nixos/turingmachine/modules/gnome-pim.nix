{ pkgs, ... }: {
  programs.dconf.enable = true;
  services.gnome.evolution-data-server.enable = true;
  services.gnome.gnome-online-accounts.enable = true;
  services.gnome.gnome-keyring.enable = true;
  environment.systemPackages = [
    pkgs.gnome.gnome-calendar
    # broken
    #pkgs.gnome.gnome-todo
    pkgs.evolution
  ];
}
