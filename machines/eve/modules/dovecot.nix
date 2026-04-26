{ pkgs, config, ... }:
{
  environment.etc."dovecot/virtual/All/dovecot-virtual".text = ''
    *
    -Trash
    -Trash.*
    -Spam
      all
  '';

  sops.templates."dovecot-ldap.conf" = {
    content = ''
      hosts = 127.0.0.1
      dn = "cn=dovecot,dc=mail,dc=eve"
      dnpass = "${config.sops.placeholder.dovecot-ldap-password}"
      tls = no
      auth_bind = yes
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
    '';
    owner = "dovecot2";
  };

  services.dovecot2 = {
    enable = true;
    enablePAM = false;
    mailPlugins = {
      globally.enable = [
        "virtual"
        "fts"
        "fts_flatcurve"
      ];
      perProtocol.lmtp.enable = [ "sieve" ];
    };
    settings = {
      protocols = [
        "imap"
        "lmtp"
        "sieve"
      ];
      mail_location = "maildir:/var/vmail/%d/%n/Maildir";
      mail_uid = "vmail";
      mail_gid = "vmail";
    };
    # The bulk of the config is still written in raw 2.3 syntax; using
    # includeFiles keeps the on-disk dovecot.conf identical to what was
    # previously emitted via extraConfig, avoiding a risky rewrite into
    # the structured settings format in one go.
    includeFiles = [
      (pkgs.writeText "dovecot-eve.conf" ''
        # Let imap processes (and sieve pipe scripts) access opencrow's
        # trigger pipe.  Dovecot drops supplementary groups when switching
        # to mail_uid, so we must list extra groups here explicitly.
        mail_access_groups = opencrow

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

        # Expose a virtual "All" mailbox so IMAP clients can run a single
        # SEARCH across every real folder (IMAP itself has no cross-mailbox
        # search, so we aggregate via the virtual plugin instead).
        namespace virtual {
          # Must match the inbox namespace separator (Maildir++ default ".")
          # or Dovecot refuses to start with visible namespaces.
          prefix = Virtual.
          separator = .
          # Listed so webmail (SnappyMail) can discover it; sync clients
          # like mbsync must exclude it via "Patterns * !Virtual*" to
          # avoid re-downloading every message.
          hidden = no
          list = yes
          subscriptions = no
          location = virtual:/etc/dovecot/virtual:INDEX=/var/vmail/%d/%n/virtual
        }

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
          args = ${config.sops.templates."dovecot-ldap.conf".path}
          driver = ldap
        }
        passdb {
          args = ${config.sops.templates."dovecot-ldap.conf".path}
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

          fts = flatcurve
          fts_autoindex = yes
          fts_enforced = yes
          # flatcurve uses lib-fts which on 2.3 hard-requires fts_languages
          # and fts_tokenizers to be set (no built-in defaults yet).
          fts_languages = en de
          fts_tokenizers = generic email-address
          fts_filters = normalizer-icu snowball
          fts_filters_en = lowercase snowball english-possessive
        }

        # If you have Dovecot v2.2.8+ you may get a significant performance improvement with fetch-headers:
        imapc_features = fetch-headers
        # Read multiple mails in parallel, improves performance
        mail_prefetch_count = 20
      '')
    ];
  };

  environment.systemPackages = [
    pkgs.dovecot_pigeonhole
    # Xapian-backed FTS plugin; replaces deprecated CLucene backend and
    # is the upstream-endorsed indexer (built-in starting with 2.4).
    pkgs.dovecot-fts-flatcurve
  ];

  users.users.vmail = {
    home = "/var/vmail";
    createHome = true;
    isSystemUser = true;
    uid = 1000;
    shell = "/run/current-system/sw/bin/nologin";
    # Allow sieve pipe scripts (running as vmail) to write to opencrow's
    # trigger pipe, used for forwarding starred emails to Janet.
    extraGroups = [ "opencrow" ];
  };

  security.dhparams = {
    enable = true;
    params.dovecot2 = { };
  };

  security.acme.certs =
    let
      cert =
        {
          domain,
          extraDomainNames ? [ ],
        }:
        {
          postRun = "systemctl --no-block restart dovecot2.service";
          group = "dovecot2";
          dnsProvider = "rfc2136";
          environmentFile = config.sops.secrets.lego-knot-credentials.path;
          inherit domain extraDomainNames;
        };
    in
    {
      "imap.thalheim.io" = cert { domain = "imap.thalheim.io"; };
      # validation for subdomain does not work, might need _acme-challenge.imap.devkid.net
      "imap.devkid.net" = cert {
        domain = "devkid.net";
        extraDomainNames = [ "*.devkid.net" ];
      };
    };

  networking.firewall.allowedTCPPorts = [
    143 # imap
    993 # imaps
    4190 # sieve
  ];
}
