{ pkgs, ... }: {
  programs.dconf.enable = true;
  services.gnome3.evolution-data-server.enable = true;
  services.gnome3.gnome-online-accounts.enable = true;
  services.gnome3.gnome-keyring.enable = true;
  environment.systemPackages = [
    pkgs.gnome3.gnome-calendar
    pkgs.gnome3.gnome-todo
    pkgs.gnome3.evolution
  ];
}
