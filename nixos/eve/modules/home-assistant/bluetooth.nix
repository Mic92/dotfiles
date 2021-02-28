{ pkgs, config, ... }:
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
          action = "connect";
          bluetooth_device = "{{ bluetooth_device }}";
          device = "{{ device }}";
        };
      };
    };
    intent_script.DisconnectBluetooth = {
      speech.text = "Disconnect {{ bluetooth_device }}.";
      action = {
        service = "python_script.connect_bluetooth";
        data_template = {
          action = "disconnect";
          bluetooth_device = "{{ bluetooth_device }}";
        };
      };
    };

    shell_command.ssh_bluetooth = ''${pkgs.openssh}/bin/ssh -i ${config.sops.secrets.ssh-homeassistant.path} hass-agent@{{ host }} "bluetoothctl {{ action }} {{ mac }}"'';
  };
}
