{
  config,
  lib,
  pkgs,
  ...
}: {
  networking.firewall.allowedTCPPorts = [2323];

  systemd.services.uptermd = {
    description = "upterm secure terminal sharing";
    wants = ["network.target"];
    after = ["network.target"];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.upterm}/bin/uptermd --ssh-addr 0.0.0.0:2323";
      IPAccounting = "yes";
      #IPAddressAllow = "localhost";
      #IPAddressDeny = "any";
      DynamicUser = "yes";
      PrivateTmp = "yes";
      PrivateUsers = "yes";
      PrivateDevices = "yes";
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = "yes";
      ProtectClock = "yes";
      ProtectControlGroups = "yes";
      ProtectKernelLogs = "yes";
      ProtectKernelModules = "yes";
      ProtectKernelTunables = "yes";
      ProtectProc = "invisible";
      CapabilityBoundingSet = "CAP_NET_BIND_SERVICE";
    };
  };
}
