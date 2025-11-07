{ pkgs, config, ... }:
{
  services.openldap = {
    enable = true;

    settings.attrs.olcLogLevel = "0";

    settings.children = {
      "cn=module" = {
        attrs = {
          objectClass = "olcModuleList";
          olcModulePath = "${pkgs.openldap}/lib/modules";
          olcModuleLoad = [
            "argon2"
            "memberof"
            "syncprov"
          ];
        };
      };

      "cn=schema".includes = [
        "${pkgs.openldap}/etc/schema/core.ldif"
        "${pkgs.openldap}/etc/schema/cosine.ldif"
        "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
        "${pkgs.openldap}/etc/schema/nis.ldif"
      ];

      "olcDatabase={-1}frontend".attrs = {
        objectClass = [
          "olcDatabaseConfig"
          "olcFrontendConfig"
        ];
        olcDatabase = "{-1}frontend";
        olcPasswordHash = "{ARGON2}";
      };

      "olcDatabase={1}mdb".attrs = {
        objectClass = [
          "olcDatabaseConfig"
          "olcMdbConfig"
        ];
        olcDatabase = "{1}mdb";
        olcDbDirectory = "/var/lib/openldap/data";
        olcRootPW.path = config.sops.secrets.openldap-rootpw.path;
        olcRootDN = "cn=admin,dc=eve";
        olcSuffix = "dc=eve";
        olcAccess = [
          # Allow admins full write access to everything
          ''
            {0}to *
                           by group.exact="cn=admins,ou=groups,dc=eve" write
                           by * break''
          ''
            {1}to attrs=userPassword
                           by self write  by anonymous auth
                           by dn.base="cn=authelia,ou=system,ou=users,dc=eve" write
                           by dn.base="cn=dovecot,dc=mail,dc=eve" read
                           by dn.base="cn=postfix,ou=system,ou=users,dc=eve" read
                           by dn.base="cn=nextcloud,ou=system,ou=users,dc=eve" read
                           by dn.base="cn=ldapsync,ou=system,ou=users,dc=eve" read
                           by dn.base="cn=phpldapadmin,ou=system,ou=users,dc=eve" read
                           by * none''
          ''{2}to attrs=loginShell  by self write  by users read''
          ''
            {3}to dn.subtree="dc=domains,dc=mail,dc=eve"
                           by dn.base="cn=postfix,ou=system,ou=users,dc=eve" read
                           by * none''
          ''
            {4}to dn.subtree="dc=aliases,dc=mail,dc=eve"
                           by dn.base="cn=postfix,ou=system,ou=users,dc=eve" read
                           by * none''
          ''
            {5}to dn.subtree="ou=users,dc=eve" attrs=mail,mailbox,maildrop,quota,cn,objectClass,memberOf
                           by dn.base="cn=postfix,ou=system,ou=users,dc=eve" read
                           by dn.base="cn=dovecot,dc=mail,dc=eve" read
                           by dn.base="cn=vaultwarden-ldap,ou=system,ou=users,dc=eve" read
                           by dn.base="cn=phpldapadmin,ou=system,ou=users,dc=eve" read
                           by users read
                           by * none''
          ''
            {6}to dn.subtree="ou=system,ou=users,dc=eve"
                           by dn.base="cn=dovecot,dc=mail,dc=eve" read
                           by dn.base="cn=nextcloud,ou=system,ou=users,dc=eve" read
                           by dn.subtree="ou=system,ou=users,dc=eve" read
                           by * none''
          ''
            {7}to *
                           by users read
                           by * none''
        ];
      };
      "olcOverlay={0}memberof,olcDatabase={1}mdb".attrs = {
        objectClass = [
          "olcOverlayConfig"
          "olcMemberOf"
        ];
        olcOverlay = "{0}memberof";
        olcMemberOfRefInt = "TRUE";
        olcMemberOfGroupOC = "groupOfNames";
        olcMemberOfMemberAD = "member";
        olcMemberOfMemberOfAD = "memberOf";
      };
      "olcOverlay={1}syncprov,olcDatabase={1}mdb".attrs = {
        objectClass = [
          "olcOverlayConfig"
          "olcSyncProvConfig"
        ];
        olcOverlay = "{1}syncprov";
        olcSpSessionLog = "100";
      };
      "olcDatabase={2}monitor".attrs = {
        olcDatabase = "{2}monitor";
        objectClass = [
          "olcDatabaseConfig"
          "olcMonitorConfig"
        ];
        olcAccess = [
          ''
            {0}to *
                         by dn.exact="cn=netdata,ou=system,ou=users,dc=eve" read
                         by * none''
        ];
      };

      "cn={1}grafana,cn=schema".attrs = {
        cn = "{1}grafana";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''
            (1.3.6.1.4.1.28293.1.2.5 NAME 'grafana'
                         SUP uidObject AUXILIARY
                         DESC 'Added to an account to allow grafana access'
                         MUST (mail))
          ''
        ];
      };
      "cn={2}postfix,cn=schema".attrs = {
        cn = "{2}postfix";
        objectClass = "olcSchemaConfig";
        olcAttributeTypes = [
          ''
            (1.3.6.1.4.1.12461.1.1.1 NAME 'postfixTransport'
                         DESC 'A string directing postfix which transport to use'
                         EQUALITY caseExactIA5Match
                         SYNTAX 1.3.6.1.4.1.1466.115.121.1.26{20} SINGLE-VALUE)''
          ''
            (1.3.6.1.4.1.12461.1.1.5 NAME 'mailbox'
                         DESC 'The absolute path to the mailbox for a mail account in a non-default location'
                         EQUALITY caseExactIA5Match
                         SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 SINGLE-VALUE)''
          ''
            (1.3.6.1.4.1.12461.1.1.6 NAME 'quota'
                         DESC 'A string that represents the quota on a mailbox'
                         EQUALITY caseExactIA5Match
                         SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 SINGLE-VALUE)''
          ''
            (1.3.6.1.4.1.12461.1.1.8 NAME 'maildrop'
                         DESC 'RFC822 Mailbox - mail alias'
                         EQUALITY caseIgnoreIA5Match
                         SUBSTR caseIgnoreIA5SubstringsMatch
                         SYNTAX 1.3.6.1.4.1.1466.115.121.1.26{256})''
        ];
        olcObjectClasses = [
          ''
            (1.3.6.1.4.1.12461.1.2.1 NAME 'mailAccount'
                         SUP top AUXILIARY
                         DESC 'Mail account objects'
                         MUST ( mail $ userPassword )
                         MAY (  cn $ description $ quota))''
          ''
            (1.3.6.1.4.1.12461.1.2.2 NAME 'mailAlias'
                         SUP top STRUCTURAL
                         DESC 'Mail aliasing/forwarding entry'
                         MUST ( mail $ maildrop )
                         MAY ( cn $ description ))''
          ''
            (1.3.6.1.4.1.12461.1.2.3 NAME 'mailDomain'
                         SUP domain STRUCTURAL
                         DESC 'Virtual Domain entry to be used with postfix transport maps'
                         MUST ( dc )
                         MAY ( postfixTransport $ description  ))''
          ''
            (1.3.6.1.4.1.12461.1.2.4 NAME 'mailPostmaster'
                         SUP top AUXILIARY
                         DESC 'Added to a mailAlias to create a postmaster entry'
                         MUST roleOccupant)''
        ];
      };

      "cn={1}nextcloud,cn=schema".attrs = {
        cn = "{1}nextcloud";
        objectClass = "olcSchemaConfig";
        olcAttributeTypes = [
          ''
            (1.3.6.1.4.1.39430.1.1.1
                         NAME 'ownCloudQuota'
                         DESC 'User Quota (e.g. 15 GB)'
                         SYNTAX '1.3.6.1.4.1.1466.115.121.1.15')''
        ];
        olcObjectClasses = [
          ''
            (1.3.6.1.4.1.39430.1.2.1
                         NAME 'ownCloud'
                         DESC 'ownCloud LDAP Schema'
                         AUXILIARY
                         MUST ( mail $ userPassword )
                         MAY ( ownCloudQuota ))''
        ];
      };
      "cn={1}prometheus,cn=schema".attrs = {
        cn = "{1}prometheus";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''
            (1.3.6.1.4.1.28296.1.2.4
                         NAME 'prometheus'
                         SUP uidObject AUXILIARY
                         DESC 'Added to an account to allow prometheus access'
                         MUST (mail))
          ''
        ];
      };
      "cn={1}loki,cn=schema".attrs = {
        cn = "{1}loki";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''
            (1.3.6.1.4.1.28299.1.2.4
                         NAME 'loki'
                         SUP uidObject AUXILIARY
                         DESC 'Added to an account to allow loki access'
                         MUST (mail))
          ''
        ];
      };

    };
  };

  sops.secrets.openldap-rootpw.owner = "openldap";

  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 389 ];
}
