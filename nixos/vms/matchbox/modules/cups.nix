{ pkgs, ... }: {
  services.printing = {
    enable = true;
    browsing = true;
    listenAddresses = [
      "172.23.75.254:631"
      "127.0.0.1:631"
    ];
    defaultShared = true;
    drivers = [ pkgs.hplip ];
  };

  nixpkgs.config.packageOverrides = pkgs: {
    hplip = pkgs.hplip.override {
      withQt5 = false;
    };
  };

  fonts.enableGhostscriptFonts = true;

  networking.firewall.allowedTCPPorts = [ 631 ];
  networking.firewall.allowedUDPPorts = [ 631 ];
  services.avahi = {
    enable = true;
    publish.enable = true;
    publish.userServices = true;
  };
}
