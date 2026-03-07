{
  config,
  self,
  pkgs,
  ...
}:
let
  dotfiles = "${self}/home";
in
{
  services.opencrow.skills.calendar = ./skills/calendar;

  clan.core.vars.generators.opencrow-nextcloud = {
    files.nextcloud-thalheim-password.secret = true;
    files.nextcloud-clan-password.secret = true;

    prompts.nextcloud-thalheim-password.description = "Nextcloud app password for cloud.thalheim.io (joerg@thalheim.io)";
    prompts.nextcloud-clan-password.description = "Nextcloud app password for nextcloud.clan.lol (Mic92)";

    script = ''
      cp "$prompts/nextcloud-thalheim-password" "$out/nextcloud-thalheim-password"
      cp "$prompts/nextcloud-clan-password" "$out/nextcloud-clan-password"
    '';
  };

  services.opencrow.rbwEntries."Eve" = "nextcloud-thalheim-password";
  services.opencrow.rbwEntries."nextcloud.clan.lol Mic92" = "nextcloud-clan-password";

  services.opencrow.credentialFiles."nextcloud-thalheim-password" =
    config.clan.core.vars.generators.opencrow-nextcloud.files.nextcloud-thalheim-password.path;
  services.opencrow.credentialFiles."nextcloud-clan-password" =
    config.clan.core.vars.generators.opencrow-nextcloud.files.nextcloud-clan-password.path;

  services.opencrow.extraPackages = [
    (pkgs.runCommand "vdirsyncer-hooks" { } ''
      mkdir -p $out/bin
      cp ${dotfiles}/bin/vdirsyncer-post-hook $out/bin/
      cp ${dotfiles}/bin/vdirsyncer-pre-deletion-hook $out/bin/
      chmod +x $out/bin/*
    '')
    pkgs.khal
    pkgs.todoman
    pkgs.vdirsyncer
  ];

  containers.opencrow.config.systemd.tmpfiles.rules = [
    "d /var/lib/opencrow/.config/vdirsyncer 0750 opencrow opencrow -"
    "L+ /var/lib/opencrow/.config/vdirsyncer/config - - - - ${dotfiles}/.config/vdirsyncer/config"
    "d /var/lib/opencrow/.config/khal 0750 opencrow opencrow -"
    "L+ /var/lib/opencrow/.config/khal/config - - - - ${dotfiles}/.config/khal/config"
    "d /var/lib/opencrow/.config/todoman 0750 opencrow opencrow -"
    "L+ /var/lib/opencrow/.config/todoman/config.py - - - - ${dotfiles}/.config/todoman/config.py"
    "L+ /var/lib/opencrow/.config/todoman/__init__.py - - - - ${dotfiles}/.config/todoman/__init__.py"
  ];
}
