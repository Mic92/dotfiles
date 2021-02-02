{ pkgs, config, ...}:
{
  sops.secrets.ssh-homeassistant.owner = "hass";
  users.users.hass.extraGroups = [ "keys" ];
  systemd.services.home-assistant.serviceConfig.SupplementaryGroups = [ "keys" ];

  services.home-assistant.config = {
    intent_script.ConnectBluetooth = {
      speech.text = "Connect {{ bluetooth_device }} to {{ device }}.";
      action = {
        service = "python_script.connect_bluetooth";
        data_template = {
          bluetooth_device = "{{ bluetooth_device }}";
          device = "{{ device }}";
        };
      };
    };

    shell_command.ssh_bluetooth = ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} bluetooth@{{ host }} "bluetoothctl {{ action }} {{ mac }}"'';
  };
}
