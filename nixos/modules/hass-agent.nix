{
  users.users.hass-agent = {
    isSystemUser = true;
    group = "hass-agent";
    shell = "/run/current-system/sw/bin/bash";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA6vG7qFCKcdlB+0PdLc2IY7dBmD26NcSEUVwaoqLFNB"
    ];
  };

  users.groups.hass-agent = { };

  security.sudo.extraRules = [
    {
      users = [ "hass-agent" ];
      commands = [
        {
          command = "/run/current-system/sw/bin/systemctl suspend";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];
}
