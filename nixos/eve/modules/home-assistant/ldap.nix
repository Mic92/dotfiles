{ pkgs, config, ... }: let
  ldap-auth-sh = pkgs.callPackage ./ldap-auth-sh.nix {
    ldapPasswordFile = config.sops.secrets.home-assistant-ldap.path;
  };
in {
  services.home-assistant.config.homeassistant.auth_providers = [{
    type = "command_line";
    command = "${ldap-auth-sh}/bin/ldap-auth.sh";
    meta = true;
  }];

  services.openldap.extraConfig = ''
    objectClass ( 1.3.6.1.4.1.28297.1.2.4 NAME 'homeAssistant'
            SUP uidObject AUXILIARY
            DESC 'Added to an account to allow home-assistant access'
            MUST (mail) )
  '';

  sops.secrets.home-assistant-ldap.owner = "hass";
}
