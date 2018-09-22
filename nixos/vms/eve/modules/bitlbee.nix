{ pkgs, ... }: {
  services.bitlbee = {
    enable = true;
    interface = "0.0.0.0";
    authMode = "Registered";
    authBackend = "pam";
    libpurple_plugins = with pkgs; [
      purple-hangouts 
      purple-matrix 
      (pkgs.callPackage ../pkgs/purple-skypeweb.nix {})
    ];
  };
}
