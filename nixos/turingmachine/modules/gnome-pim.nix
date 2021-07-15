{ pkgs, ... }: {
  programs.dconf.enable = true;
  services.gnome.evolution-data-server.enable = true;
  services.gnome.gnome-online-accounts.enable = true;
  services.gnome.gnome-keyring.enable = true;
  environment.systemPackages = [
    pkgs.gnome3.gnome-calendar
    # broken
    #pkgs.gnome3.gnome-todo
    pkgs.evolution
  ];
}
