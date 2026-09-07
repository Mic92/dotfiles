{ config, pkgs, ... }:
{
  clan.core.vars.generators.storagebox-smb = {
    files.credentials = { };
    prompts.password.description = "Hetzner storage box u664466-sub1 SMB password";
    script = ''
      printf 'username=u664466-sub1\npassword=%s\n' "$(cat "$prompts/password")" > "$out/credentials"
    '';
  };

  environment.systemPackages = [ pkgs.cifs-utils ];

  fileSystems."/mnt/storagebox" = {
    device = "//u664466-sub1.your-storagebox.de/u664466-sub1";
    fsType = "cifs";
    options = [
      "credentials=${config.clan.core.vars.generators.storagebox-smb.files.credentials.path}"
      "seal"
      "vers=3.1.1"
      "uid=0"
      "gid=0"
      "file_mode=0660"
      "dir_mode=0770"
      "_netdev"
      "nofail"
      "x-systemd.automount"
      "x-systemd.idle-timeout=10min"
      "x-systemd.mount-timeout=30s"
    ];
  };
}
