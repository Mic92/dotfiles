{ pkgs, lib, ... }: let
  ldap-auth-sh = pkgs.stdenv.mkDerivation {
    name = "ldap-auth-sh";
    src = pkgs.fetchFromGitHub {
      owner = "efficiosoft";
      repo = "ldap-auth-sh";
      rev = "93b2c00413942908139e37c7432a12bcb705ac87";
      sha256 = "1pymp6ki353aqkigr89g7hg5x1mny68m31c3inxf1zr26n5s2kz8";
    };
    nativeBuildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      mkdir -p $out/etc
      cat > $out/etc/home-assistant.cfg << 'EOF'
      CLIENT="ldapsearch"
      SERVER="ldap://localhost:389"
      USERDN="cn=home-assistant,ou=system,ou=users,dc=eve"
      PW="$(cat /run/keys/home-assistant-ldap)"

      BASEDN="ou=users,dc=eve"
      SCOPE="one"
      NAME_ATTR="cn"
      FILTER="(&(objectClass=homeAssistant)(mail=$(ldap_dn_escape "$username")))"
      USERNAME_PATTERN='^[a-z|A-Z|0-9|_|-|.|@]+$'
      on_auth_success() {
        # print the meta entries for use in HA
        if [ ! -z "$NAME_ATTR" ]; then
          name=$(echo "$output" | sed -nr "s/^\s*$NAME_ATTR:\s*(.+)\s*\$/\1/Ip")
          [ -z "$name" ] || echo "name=$name"
        fi
      }
      EOF
      install -D -m755 ldap-auth.sh $out/bin/ldap-auth.sh
      wrapProgram $out/bin/ldap-auth.sh \
        --prefix PATH : ${lib.makeBinPath [ pkgs.openldap pkgs.coreutils pkgs.gnused pkgs.gnugrep ]} \
        --add-flags "$out/etc/home-assistant.cfg"
    '';
  };
in {
  services.home-assistant.config = {
    frontend = {};
    http = {};
    homeassistant = {
      auth_providers = [{
        type = "command_line";
        command = "${ldap-auth-sh}/bin/ldap-auth.sh";
      }];
    };
  };

  krops.secrets.files.home-assistant-ldap.owner = "hass";
  services.openldap.extraConfig = ''
    objectClass ( 1.3.6.1.4.1.28297.1.2.4 NAME 'homeAssistant'
            SUP uidObject AUXILIARY
            DESC 'Added to an account to allow home-assistant access'
            MUST (mail) )
  '';
}
