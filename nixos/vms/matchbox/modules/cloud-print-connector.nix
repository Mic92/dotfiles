{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    # for first time setup with gcp-connector-util
    cloud-print-connector
  ];

  systemd.services.cloud-print-connector = {
    description = "Google Cloud Print Connector";
    documentation = ["https://github.com/google/cloud-print-connector"];
    wantedBy = [ "multi-user.target" ];
    after = [ "cups.service" "avahi.service" "network.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.cloud-print-connector}/bin/gcp-cups-connector -config-filename /run/keys/gcp-cups-connector.config.json";
      Restart = "on-failure";
      User = "cloud-print-connector";
      SupplementaryGroups = [ "keys" ];
    };
  };

  # generated with:
  # $ gcp-connector-util init
  deployment.keys."gcp-cups-connector.config.json" = {
    keyFile = ../secrets/gcp-cups-connector.config.json;
    user = "cloud-print-connector";
  };

  users.users.cloud-print-connector = {
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/cloud-print-connector";
    group = "cloud-print-connector";
    extraGroups = [ "keys" ];
  };

  users.groups.cloud-print-connector = {};
}
