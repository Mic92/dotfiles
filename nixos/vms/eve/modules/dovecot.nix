{ pkgs, config, ... }: 
let
  ldapConfig = pkgs.writeText "dovecot-ldap.conf" ''
    hosts = 127.0.0.1
    dn = "cn=dovecot,dc=mail,dc=eve"
    dnpass = "@ldap-password@"
    tls = no
    auth_bind = no
    ldap_version = 3
    base = ou=users,dc=eve
    user_filter = (&(objectClass=MailAccount)(mail=%u)(accountActive=TRUE)(delete=FALSE))
    user_attrs = \
      quota=quota_rule=*:bytes=%$, \
      =home=/var/vmail/%d/%n/, \
      =mail=maildir:/var/vmail/%d/%n/Maildir
    pass_attrs = mail=user,userPassword=password
    pass_filter = (&(objectClass=MailAccount)(mail=%u))
    iterate_attrs = =user=%{ldap:mail}
    iterate_filter = (objectClass=MailAccount)
    scope = subtree
    default_pass_scheme = SSHA
  '';
in {

  services.dovecot2 = {
    enable = true;
    enableImap = true;
    enableLmtp = true;
    mailLocation = "maildir:/var/vmail/%d/%n/Maildir";
    mailUser = "vmail";
    mailGroup = "vmail";
    extraConfig = ''
      ssl = yes
      ssl_cert = </var/lib/acme/imap.thalheim.io/fullchain.pem
      ssl_key = </var/lib/acme/imap.thalheim.io/key.pem
      local_name thalheim.io {
        ssl_cert = </var/lib/acme/thalheim.io/fullchain.pem
        ssl_key = </var/lib/acme/thalheim.io/key.pem
      }
      local_name higgsboson.tk {
        ssl_cert = </var/lib/acme/higgsboson.tk/fullchain.pem
        ssl_key = </var/lib/acme/higgsboson.tk/key.pem
      }
      local_name imap.higgsboson.tk {
        ssl_cert = </var/lib/acme/imap.higgsboson.tk/fullchain.pem
        ssl_key = </var/lib/acme/imap.higgsboson.tk/key.pem
      }
      local_name devkid.net {
        ssl_cert = </var/lib/acme/devkid.net/fullchain.pem
        ssl_key = </var/lib/acme/devkid.net/key.pem
      }
      local_name imap.devkid.net {
        ssl_cert = </var/lib/acme/imap.devkid.net/fullchain.pem
        ssl_key = </var/lib/acme/imap.devkid.net/key.pem
      }
      ssl_cipher_list = AES128+EECDH:AES128+EDH
      ssl_prefer_server_ciphers = yes
      ssl_dh=<${config.security.dhparams.params.dovecot2.path}

      mail_plugins = virtual fts fts_lucene old_stats

      service lmtp {
        user = vmail
        unix_listener /var/lib/postfix/queue/private/dovecot-lmtp {
          group = postfix
          mode = 0600
          user = postfix
        }
      }

      service doveadm {
        inet_listener {
          port = 4170
          ssl = yes
        }
      }
      protocol lmtp {
        postmaster_address=postmaster@higgsboson.tk
        hostname=mail.higgsboson.tk
        mail_plugins = $mail_plugins sieve
      }
      service auth {
        unix_listener auth-userdb {
          mode = 0640
          user = vmail
          group = vmail
        }
        # Postfix smtp-auth
        unix_listener /var/lib/postfix/queue/private/auth {
          mode = 0666
          user = postfix
          group = postfix
        }
      }
      userdb {
        args = /run/dovecot2/ldap.conf
        driver = ldap
      }
      passdb {
        args = /run/dovecot2/ldap.conf
        driver = ldap
      }

      service imap-login {
        client_limit = 1000
        service_count = 0
        inet_listener imaps {
          port = 993
        }
      }

      service managesieve-login {
        inet_listener sieve {
          port = 4190
        }
      }
      protocol sieve {
        managesieve_logout_format = bytes ( in=%i : out=%o )
      }
      plugin {
        sieve_dir = /var/vmail/%d/%n/sieve/scripts/
        sieve = /var/vmail/%d/%n/sieve/active-script.sieve
        sieve_extensions = +vacation-seconds
        sieve_vacation_min_period = 1min

        fts = lucene
        fts_lucene = whitespace_chars=@.
      }

      # If you have Dovecot v2.2.8+ you may get a significant performance improvement with fetch-headers:
      imapc_features = $imapc_features fetch-headers
      # Read multiple mails in parallel, improves performance
      mail_prefetch_count = 20

      plugin {
        # how often to session statistics (must be set)
        old_stats_refresh = 30 secs
        # track per-IMAP command statistics (optional)
        old_stats_track_cmds = yes
      }

      # netdata
      service old-stats {
        unix_listener stats {
          user = vmail
          group = vmail
          mode = 0664
        }
      }
    '';
    modules = [
      pkgs.dovecot_pigeonhole
    ];
    protocols = [
      "sieve"
    ];
  };

  users.users.vmail = {
    home = "/var/vmail";
    createHome = true;
    isSystemUser = true;
    uid = 1000;
    shell = "/run/current-system/sw/bin/nologin";
  };

  deployment.keys."dovecot-ldap-password" = {
    keyFile = ../secrets/dovecot-ldap-password;
  };

  security.dhparams = {
    enable = true;
    params.dovecot2 = {};
  };

  systemd.services.dovecot2.preStart = ''
    sed -e "s!@ldap-password@!$(cat /run/keys/dovecot-ldap-password)!" ${ldapConfig} > /run/dovecot2/ldap.conf
  '';

  security.acme.certs = let
    cert = {
      postRun = "systemctl restart dovecot2.service";
      webroot = "/var/lib/acme/acme-challenge";
      allowKeysForGroup = true;
      group = "dovecot2";
    };
  in {
    "imap.higgsboson.tk" = cert;
    "imap.thalheim.io" = cert;
    "imap.devkid.net" = cert;
  };

  services.netdata.portcheck.checks = {
    dovecot-imap.port = 143;
    dovecot-imaps.port = 993;
    dovecot-sieve.port = 4190;
  };

  networking.firewall.allowedTCPPorts = [
    143 # imap
    993 # imaps
    4190 # sieve
  ];

  environment.etc."netdata/python.d/dovecot.conf".text = ''
    localsocket2:
      name : 'local'
      socket : '/run/dovecot2/stats'
  '';

  users.users.netdata.extraGroups = [ "vmail" ];
}
