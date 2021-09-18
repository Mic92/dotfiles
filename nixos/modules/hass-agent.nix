{ pkgs, ... }:
{
  users.extraUsers.hass-agent = {
    isSystemUser = true;
    group = "hass-agent";
    shell = "/run/current-system/sw/bin/bash";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA6vG7qFCKcdlB+0PdLc2IY7dBmD26NcSEUVwaoqLFNB"
    ];
    packages = [
      (pkgs.writeShellScriptBin "mpv-play" ''
        export XDG_RUNTIME_DIR=/run/user/1000
        ${pkgs.coreutils}/bin/nohup ${pkgs.mpv}/bin/mpv -- "$@" < /dev/null >/dev/null 2>&1 &
      '')
    ];
  };

  users.groups.hass-agent = {};

  security.sudo.extraRules = [{
    users = [ "hass-agent" ];
    commands = [{
      command = "/run/current-system/sw/bin/systemctl suspend";
      options = [ "NOPASSWD" ];
    }];
  }
    {
      users = [ "hass-agent" ];
      runAs = "joerg";
      commands = [{
        command = "/etc/profiles/per-user/hass-agent/bin/mpv-play";
        options = [ "NOPASSWD" ];
      }];
    }];
}
