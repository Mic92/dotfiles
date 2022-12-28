{ pkgs
, config
, ...
}:
let
  ldap-auth-sh = pkgs.callPackage ./ldap-auth-sh.nix {
    ldapPasswordFile = config.sops.secrets.home-assistant-ldap.path;
  };
in
{
  services.home-assistant.config.homeassistant.auth_providers = [
    {
      type = "command_line";
      command = "${ldap-auth-sh}/bin/ldap-auth.sh";
      meta = true;
    }
  ];

  sops.secrets.home-assistant-ldap.owner = "hass";
}
