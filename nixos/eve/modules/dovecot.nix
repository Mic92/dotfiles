{ pkgs
, config
, ...
}:
let
  ldapConfig = pkgs.writeText "dovecot-ldap.conf" ''
    hosts = 127.0.0.1
    dn = "cn=dovecot,dc=mail,dc=eve"
    dnpass = "@ldap-password@"
    tls = no
    auth_bind = no
    ldap_version = 3
    base = ou=users,dc=eve
    user_filter = (&(objectClass=mailAccount)(mail=%u))
    user_attrs = \
      quota=quota_rule=*:bytes=%$, \
      =home=/var/vmail/%d/%n/, \
      =mail=maildir:/var/vmail/%d/%n/Maildir
    pass_attrs = mail=user,userPassword=password
    pass_filter = (&(objectClass=mailAccount)(mail=%u))
    iterate_attrs = =user=%{ldap:mail}
    iterate_filter = (objectClass=mailAccount)
    scope = subtree
    default_pass_scheme = SSHA
  '';
in
{
  services.dovecot2 = {
    enable = true;
    enableImap = true;
    enableLmtp = true;
    enablePAM = false;
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
      local_name devkid.net {
        ssl_cert = </var/lib/acme/devkid.net/fullchain.pem
        ssl_key = </var/lib/acme/devkid.net/key.pem
      }
      local_name imap.devkid.net {
        ssl_cert = </var/lib/acme/imap.devkid.net/fullchain.pem
        ssl_key = </var/lib/acme/imap.devkid.net/key.pem
      }
      ssl_min_protocol = TLSv1.2
      ssl_cipher_list = EECDH+AESGCM:EDH+AESGCM
      ssl_prefer_server_ciphers = yes
      ssl_dh=<${config.security.dhparams.params.dovecot2.path}

      mail_plugins = virtual fts fts_lucene

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
        postmaster_address=postmaster@thalheim.io
        hostname=mail.thalheim.io
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

  security.dhparams = {
    enable = true;
    params.dovecot2 = { };
  };

  systemd.services.dovecot2.preStart = ''
    sed -e "s!@ldap-password@!$(<${config.sops.secrets.dovecot-ldap-password.path})!" ${ldapConfig} > /run/dovecot2/ldap.conf
  '';

  security.acme.certs =
    let
      cert =
        { domain
        , extraDomainNames ? [ ]
        ,
        }: {
          postRun = "systemctl restart dovecot2.service";
          group = "dovecot2";
          dnsProvider = "rfc2136";
          credentialsFile = config.sops.secrets.lego-knot-credentials.path;
          inherit domain extraDomainNames;
        };
    in
    {
      "imap.thalheim.io" = cert {
        domain = "imap.thalheim.io";
      };
      # validation for subdomain does not work, might need _acme-challenge.imap.devkid.net
      "imap.devkid.net" = cert {
        domain = "devkid.net";
        extraDomainNames = [
          "*.devkid.net"
        ];
      };
    };

  networking.firewall.allowedTCPPorts = [
    143 # imap
    993 # imaps
    4190 # sieve
  ];
}
