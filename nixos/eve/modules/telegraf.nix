{ pkgs, ... }: {
  imports = [
    ../../modules/telegraf.nix
  ];
  systemd.services.fix-telegraf-postfix-access = {
    wantedBy = [ "multi-user.target" ];
    requiredBy = [ "telegraf.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      ExecStart = [
        "${pkgs.acl}/bin/setfacl -Rm u:telegraf:rX /var/lib/postfix/"
        "${pkgs.acl}/bin/setfacl -dm u:telegraf:rX /var/lib/postfix/"
      ];
    };
  };

  services.telegraf.extraConfig.inputs = {
    postfix.queue_directory = "/var/lib/postfix/queue";
    ping = let
      urls = [
        "eva.r"
        "eva.thalheim.io"
      ];
      mobileUrls = [
        "turingmachine.r"
        "herbert.r"
      ];
    in [{
      method = "native";
      urls = map (url: "${url}") mobileUrls;
      tags.type = "mobile";
      count = 5;
    } {
      method = "native";
      urls = map (url: "4.${url}") urls;
    } {
      method = "native";
      urls = map (url: "6.${url}") urls;
      ipv6 = true;
    }];
    smart.path = "${pkgs.smartmontools}/bin/smartctl";
    smart.use_sudo = true;
  };

  systemd.services.telegraf.path = [ "/run/wrappers" ];

  security.sudo.extraRules = [{
    users = [ "telegraf" ];
    commands = [ {
      command = "${pkgs.smartmontools}/bin/smartctl";
      options = [ "NOPASSWD" ];
    }];
  }];
}
