{ pkgs
, config
, ...
}: {
  services.openldap = {
    enable = true;

    settings.attrs.olcLogLevel = "0";

    settings.children = {
      "cn=schema".includes = [
        "${pkgs.openldap}/etc/schema/core.ldif"
        "${pkgs.openldap}/etc/schema/cosine.ldif"
        "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
        "${pkgs.openldap}/etc/schema/nis.ldif"
      ];

      "olcDatabase={1}mdb".attrs = {
        objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
        olcDatabase = "{1}mdb";
        olcDbDirectory = "/var/lib/openldap/data";
        olcRootPW.path = config.sops.secrets.openldap-rootpw.path;
        olcRootDN = "cn=admin,dc=eve";
        olcSuffix = "dc=eve";
        olcAccess = [
          ''            {0}to attrs=userPassword
                           by self write  by anonymous auth
                           by dn.base="cn=dovecot,dc=mail,dc=eve" read
                           by dn.base="cn=gitlab,ou=system,ou=users,dc=eve" read
                           by dn.base="cn=ldapsync,ou=system,ou=users,dc=eve"
                           read by * none''
          ''{1}to attrs=loginShell  by self write  by * read''
          ''            {2}to dn.subtree="ou=system,ou=users,dc=eve"
                           by dn.base="cn=dovecot,dc=mail,dc=eve" read
                           by dn.subtree="ou=system,ou=users,dc=eve" read
                           by * none''
          ''{3}to dn.subtree="ou=jabber,ou=users,dc=eve"  by dn.base="cn=prosody,ou=system,ou=users,dc=eve" write  by * read''
          ''{4}to * by * read''
        ];
      };
      "olcOverlay=syncprov,olcDatabase={1}mdb".attrs = {
        objectClass = [ "olcOverlayConfig" "olcSyncProvConfig" ];
        olcOverlay = "syncprov";
        olcSpSessionLog = "100";
      };
      "olcDatabase={2}monitor".attrs = {
        olcDatabase = "{2}monitor";
        objectClass = [ "olcDatabaseConfig" "olcMonitorConfig" ];
        olcAccess = [
          ''{0}to *
             by dn.exact="cn=netdata,ou=system,ou=users,dc=eve" read
             by * none''
        ];
      };

      "cn={1}bitwarden,cn=schema" = {
        attrs = {
          cn = "{1}bitwarden";
          objectClass = "olcSchemaConfig";
          olcObjectClasses = "(1.3.6.1.4.1.28298.1.2.4 NAME 'bitwarden'
            SUP uidObject AUXILIARY
            DESC 'Added to an account to allow bitwarden access'
            MUST (mail $ userPassword))";
        };
      };
      "cn={1}squid,cn=schema".attrs = {
        cn = "{1}squid";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.16548.1.2.4 NAME 'proxyUser'
            SUP top AUXILIARY
            DESC 'Account to allow a user to use the Squid proxy'
            MUST ( mail $ userPassword ))
          ''
        ];
      };
      "cn={1}grafana,cn=schema".attrs = {
        cn = "{1}grafana";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.28293.1.2.5 NAME 'grafana'
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
          ''(1.3.6.1.4.1.12461.1.1.1 NAME 'postfixTransport'
             DESC 'A string directing postfix which transport to use'
             EQUALITY caseExactIA5Match
             SYNTAX 1.3.6.1.4.1.1466.115.121.1.26{20} SINGLE-VALUE)''
          ''(1.3.6.1.4.1.12461.1.1.5 NAME 'mailbox'
             DESC 'The absolute path to the mailbox for a mail account in a non-default location'
             EQUALITY caseExactIA5Match
             SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 SINGLE-VALUE)''
          ''(1.3.6.1.4.1.12461.1.1.6 NAME 'quota'
             DESC 'A string that represents the quota on a mailbox'
             EQUALITY caseExactIA5Match
             SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 SINGLE-VALUE)''
          ''(1.3.6.1.4.1.12461.1.1.8 NAME 'maildrop'
             DESC 'RFC822 Mailbox - mail alias'
             EQUALITY caseIgnoreIA5Match
             SUBSTR caseIgnoreIA5SubstringsMatch
             SYNTAX 1.3.6.1.4.1.1466.115.121.1.26{256})''
        ];
        olcObjectClasses = [
          ''(1.3.6.1.4.1.12461.1.2.1 NAME 'mailAccount'
             SUP top AUXILIARY
             DESC 'Mail account objects'
             MUST ( mail $ userPassword )
             MAY (  cn $ description $ quota))''
          ''(1.3.6.1.4.1.12461.1.2.2 NAME 'mailAlias'
             SUP top STRUCTURAL
             DESC 'Mail aliasing/forwarding entry'
             MUST ( mail $ maildrop )
             MAY ( cn $ description ))''
          ''(1.3.6.1.4.1.12461.1.2.3 NAME 'mailDomain'
             SUP domain STRUCTURAL
             DESC 'Virtual Domain entry to be used with postfix transport maps'
             MUST ( dc )
             MAY ( postfixTransport $ description  ))''
          ''(1.3.6.1.4.1.12461.1.2.4 NAME 'mailPostmaster'
             SUP top AUXILIARY
             DESC 'Added to a mailAlias to create a postmaster entry'
             MUST roleOccupant)''
        ];
      };
      "cn={1}openssh,cn=schema".attrs = {
        cn = "{1}openssh";
        objectClass = "olcSchemaConfig";
        olcAttributeTypes = [
          ''(1.3.6.1.4.1.24552.500.1.1.1.13
             NAME 'sshPublicKey'
             DESC 'MANDATORY: OpenSSH Public key'
             EQUALITY octetStringMatch
             SYNTAX 1.3.6.1.4.1.1466.115.121.1.40 )''
        ];
        olcObjectClasses = [
          ''(1.3.6.1.4.1.24552.500.1.1.2.0
             NAME 'ldapPublicKey'
             SUP top AUXILIARY
             DESC 'MANDATORY: OpenSSH LPK objectclass'
             MUST ( sshPublicKey $ uid ))
          ''
        ];
      };
      "cn={1}nginx,cn=schema".attrs = {
        cn = "{1}nginx";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.28295.1.2.4 NAME 'nginx'
             SUP top AUXILIARY
             DESC 'Added to an account to allow nginx access'
             MUST ( mail $ userPassword ))
          ''
        ];
      };

      "cn={1}nextcloud,cn=schema".attrs = {
        cn = "{1}nextcloud";
        objectClass = "olcSchemaConfig";
        olcAttributeTypes = [
          ''(1.3.6.1.4.1.39430.1.1.1
             NAME 'ownCloudQuota'
             DESC 'User Quota (e.g. 15 GB)'
             SYNTAX '1.3.6.1.4.1.1466.115.121.1.15')''
        ];
        olcObjectClasses = [
          ''(1.3.6.1.4.1.39430.1.2.1
             NAME 'ownCloud'
             DESC 'ownCloud LDAP Schema'
             AUXILIARY
             MUST ( mail $ userPassword )
             MAY ( ownCloudQuota ))''
        ];
      };
      "cn={1}gitlab,cn=schema".attrs = {
        cn = "{1}gitlab";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.28293.1.2.4 NAME 'gitlab'
             SUP uidObject AUXILIARY
             DESC 'Added to an account to allow gitlab access'
             MUST (mail))
          ''
        ];
      };
      "cn={1}ejabberd,cn=schema".attrs = {
        cn = "{1}ejabberd";
        objectClass = "olcSchemaConfig";
        olcAttributeTypes = [
          ''(1.2.752.43.9.1.1
             NAME 'jabberID'
             DESC 'The Jabber ID(s) associated with this object. Used to map a JID to an LDAP account.'
             EQUALITY caseIgnoreMatch
             SYNTAX 1.3.6.1.4.1.1466.115.121.1.15)
          ''
        ];
      };
      "cn={2}ejabberd,cn=schema".attrs = {
        cn = "{2}ejabberd";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.2.752.43.9.2.1
             NAME 'jabberUser'
             DESC 'A jabber user'
             AUXILIARY
             MUST ( jabberID ))
          ''
        ];
      };
      "cn={1}homeAssistant,cn=schema".attrs = {
        cn = "{1}homeAssistant";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.28297.1.2.4 NAME 'homeAssistant'
             SUP uidObject AUXILIARY
             DESC 'Added to an account to allow home-assistant access'
             MUST (mail) )
          ''
        ];
      };
      "cn={1}ttrss,cn=schema".attrs = {
        cn = "{1}ttrss";
        objectClass = "olcSchemaConfig";
        olcObjectClasses =
          ''(1.3.6.1.4.1.28294.1.2.4 NAME 'ttrss'
            SUP top AUXILIARY
            DESC 'Added to an account to allow tinytinyrss access'
            MUST ( mail $ userPassword ))'';
      };
      "cn={1}prometheus,cn=schema".attrs = {
        cn = "{1}prometheus";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.28296.1.2.4
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
          ''(1.3.6.1.4.1.28299.1.2.4
             NAME 'loki'
             SUP uidObject AUXILIARY
             DESC 'Added to an account to allow loki access'
             MUST (mail))
          ''
        ];
      };

      "cn={1}flood,cn=schema".attrs = {
        cn = "{1}flood";
        objectClass = "olcSchemaConfig";
        olcObjectClasses = [
          ''(1.3.6.1.4.1.28300.1.2.4 NAME 'flood'
             SUP uidObject AUXILIARY
             DESC 'Added to an account to allow flood access'
             MUST (mail))
          ''
        ];
      };
    };
  };

  sops.secrets.openldap-rootpw.owner = "openldap";

  networking.firewall.interfaces."tinc.retiolum".allowedTCPPorts = [ 389 ];
}
