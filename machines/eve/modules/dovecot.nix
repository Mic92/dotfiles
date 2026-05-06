{ pkgs, config, ... }:
let
  dovecot = config.services.dovecot2.package;
in
{
  environment.etc."dovecot/virtual/All/dovecot-virtual".text = ''
    *
    -Trash
    -Trash.*
    -Spam
      all
  '';

  # Rendered via sops so the bind password stays out of the nix store.
  sops.templates."dovecot-ldap.conf" = {
    content = ''
      ldap_uris = ldap://127.0.0.1
      ldap_auth_dn = cn=dovecot,dc=mail,dc=eve
      ldap_auth_dn_password = ${config.sops.placeholder.dovecot-ldap-password}
      ldap_base = ou=users,dc=eve
      ldap_scope = subtree
      ldap_version = 3

      passdb ldap {
        ldap_bind = yes
        ldap_filter = (&(objectClass=mailAccount)(mail=%{user}))
        fields {
          user = %{ldap:mail}
        }
      }

      userdb ldap {
        ldap_filter = (&(objectClass=mailAccount)(mail=%{user}))
        fields {
          home = /var/vmail/%{user | domain}/%{user | username}/
        }
        ldap_iterate_filter = (objectClass=mailAccount)
        iterate_fields {
          user = %{ldap:mail}
        }
      }
    '';
    owner = "dovecot2";
  };

  services.dovecot2 = {
    enable = true;
    enablePAM = false;
    package = pkgs.dovecot;
    settings = {
      dovecot_config_version = dovecot.version;
      dovecot_storage_version = "2.4.0";
      protocols = [
        "imap"
        "lmtp"
        "sieve"
      ];
      mail_uid = "vmail";
      mail_gid = "vmail";
    };
    # Raw snippet; the structured settings attrset can't express named
    # filters / nested sieve_script blocks cleanly yet.
    includeFiles = [
      (pkgs.writeText "dovecot-eve.conf" ''
        mail_driver = maildir
        mail_home   = /var/vmail/%{user | domain}/%{user | username}
        mail_path   = /var/vmail/%{user | domain}/%{user | username}/Maildir

        mail_plugins {
          virtual       = yes
          fts           = yes
          fts_flatcurve = yes
        }
        protocol imap {
          mail_plugins {
            imap_sieve = yes
          }
        }
        protocol lmtp {
          mail_plugins {
            sieve = yes
          }
          postmaster_address = postmaster@thalheim.io
          hostname           = mail.thalheim.io
        }

        # Let imap processes (and sieve pipe scripts) access opencrow's
        # trigger pipe.  Dovecot drops supplementary groups when switching
        # to mail_uid, so we must list extra groups here explicitly.
        mail_access_groups = opencrow

        ssl = yes
        ssl_server_cert_file = /var/lib/acme/imap.thalheim.io/fullchain.pem
        ssl_server_key_file  = /var/lib/acme/imap.thalheim.io/key.pem
        ssl_min_protocol = TLSv1.2
        ssl_cipher_list  = EECDH+AESGCM:EECDH+CHACHA20
        ssl_server_prefer_ciphers = server
        local_name thalheim.io {
          ssl_server_cert_file = /var/lib/acme/thalheim.io/fullchain.pem
          ssl_server_key_file  = /var/lib/acme/thalheim.io/key.pem
        }
        local_name devkid.net {
          ssl_server_cert_file = /var/lib/acme/devkid.net/fullchain.pem
          ssl_server_key_file  = /var/lib/acme/devkid.net/key.pem
        }
        local_name imap.devkid.net {
          ssl_server_cert_file = /var/lib/acme/imap.devkid.net/fullchain.pem
          ssl_server_key_file  = /var/lib/acme/imap.devkid.net/key.pem
        }

        # Expose a virtual "All" mailbox so IMAP clients can run a single
        # SEARCH across every real folder (IMAP itself has no cross-mailbox
        # search, so we aggregate via the virtual plugin instead).
        namespace virtual {
          # Must match the inbox namespace separator (Maildir++ default ".")
          # or Dovecot refuses to start with visible namespaces.
          prefix    = Virtual.
          separator = .
          # Listed so webmail (SnappyMail) can discover it; sync clients
          # like mbsync must exclude it via "Patterns * !Virtual*" to
          # avoid re-downloading every message.
          hidden        = no
          list          = yes
          subscriptions = no
          mail_driver     = virtual
          mail_path       = /etc/dovecot/virtual
          mail_index_path = /var/vmail/%{user | domain}/%{user | username}/virtual
        }

        !include ${config.sops.templates."dovecot-ldap.conf".path}

        service lmtp {
          unix_listener /var/lib/postfix/queue/private/dovecot-lmtp {
            group = postfix
            mode  = 0600
            user  = postfix
          }
        }

        service doveadm {
          inet_listener doveadm-tls {
            port = 4170
            ssl  = yes
          }
        }

        service auth {
          unix_listener auth-userdb {
            mode  = 0640
            user  = vmail
            group = vmail
          }
          # Postfix smtp-auth
          unix_listener /var/lib/postfix/queue/private/auth {
            mode  = 0666
            user  = postfix
            group = postfix
          }
        }

        service imap-login {
          client_limit          = 1000
          restart_request_count = unlimited
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
          managesieve_logout_format = bytes ( in=%{input} : out=%{output} )
        }

        sieve_script personal {
          driver      = file
          path        = /var/vmail/%{user | domain}/%{user | username}/sieve/scripts/
          active_path = /var/vmail/%{user | domain}/%{user | username}/sieve/active-script.sieve
        }
        sieve_extensions {
          vacation-seconds = yes
        }
        sieve_vacation_min_period = 1min

        fts flatcurve {
        }
        fts_autoindex = yes
        language en {
          default = yes
          filters = lowercase snowball english-possessive
        }
        language de {
        }
        language_tokenizers = generic email-address
        language_filters    = normalizer-icu snowball

        # Read multiple mails in parallel, improves performance
        mail_prefetch_count = 20
      '')
    ];
  };

  environment.systemPackages = [
    dovecot.passthru.dovecot_pigeonhole
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
