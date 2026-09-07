{
  config,
  lib,
  pkgs,
  ...
}:
let
  mailDomains = [
    "thalheim.io"
    "devkid.net"
    "lekwati.com"
    "handi-work.co.uk"
    "davhau.com"
    "thaigersprint.org"
  ];

  dkimDomains = [
    "thalheim.io"
    "devkid.net"
    "lekwati.com"
  ];
  dkimSelector = "202609";
  dkimKey =
    domain: algo: config.clan.core.vars.generators.stalwart-dkim.files."${domain}.${algo}.key".path;

  ldapFilter = "(memberOf=cn=mail,ou=groups,dc=eve)";
  ldapPasswordFile = config.clan.core.vars.generators.stalwart.files.ldap-password.path;
  adminPasswordFile = config.clan.core.vars.generators.stalwart.files.admin-password.path;

  cfg = config.services.stalwart;
  format = pkgs.formats.toml { };
  staticConfig = format.generate "stalwart.toml" cfg.settings;
  # staticConfig + private alias map from clan vars, assembled in preStart
  runtimeConfig = "/run/stalwart/config.toml";

  acmeDir = config.security.acme.certs."thalheim.io".directory;
in
{
  services.stalwart = {
    enable = true;
    package = pkgs.stalwart_0_15;
    stateVersion = "26.05";
    credentials = {
      ldap_password = ldapPasswordFile;
      admin_password = adminPasswordFile;
    }
    // lib.listToAttrs (
      lib.concatMap (d: [
        (lib.nameValuePair "dkim-${d}.rsa" (dkimKey d "rsa"))
        (lib.nameValuePair "dkim-${d}.ed25519" (dkimKey d "ed25519"))
      ]) dkimDomains
    );

    settings = {
      # Keys read from this file instead of the DB-backed webadmin settings.
      config.local-keys = [
        "store.*"
        "directory.*"
        "tracer.*"
        "server.*"
        "!server.blocked-ip.*"
        "!server.allowed-ip.*"
        "certificate.*"
        "authentication.*"
        "storage.*"
        "lookup.*"
        "session.*"
        "queue.connection.default.*"
        "queue.tls.default.*"
        "auth.dkim.*"
        "auth.arc.*"
        "signature.*"
        "report.*"
        "email.*"
        "imap.*"
        "jmap.*"
        "sieve.*"
        "http.*"
        "webadmin.*"
        "resolver.*"
        "spam-filter.resource"
        "config.local-keys.*"
      ];

      server = {
        hostname = "mail.thalheim.io";
        tls.certificate = "wildcard";
        http = {
          url = "'https://jmap.thalheim.io'";
          use-x-forwarded = true;
        };
        listener = {
          smtp = {
            bind = [ "[::]:25" ];
            protocol = "smtp";
          };
          submissions = {
            bind = [ "[::]:465" ];
            protocol = "smtp";
            tls.implicit = true;
          };
          submission = {
            bind = [ "[::]:587" ];
            protocol = "smtp";
          };
          imaps = {
            bind = [ "[::]:993" ];
            protocol = "imap";
            tls.implicit = true;
          };
          sieve = {
            bind = [ "[::]:4190" ];
            protocol = "managesieve";
          };
          http = {
            bind = [ "127.0.0.1:8642" ];
            protocol = "http";
          };
        };
      };

      certificate.wildcard = {
        cert = "%{file:${acmeDir}/fullchain.pem}%";
        private-key = "%{file:${acmeDir}/key.pem}%";
        default = true;
      };

      authentication.fallback-admin = {
        user = "admin";
        secret = "%{file:/run/credentials/stalwart.service/admin_password}%";
      };

      storage.directory = "ldap";
      directory.ldap = {
        type = "ldap";
        url = "ldap://127.0.0.1:3890";
        base-dn = "ou=people,dc=eve";
        timeout = "15s";
        tls.enable = false;
        bind = {
          dn = "uid=stalwart,ou=people,dc=eve";
          secret = "%{file:/run/credentials/stalwart.service/ldap_password}%";
          # lldap exposes no hashes, so look up the DN and bind as it
          auth.method = "lookup";
        };
        filter = {
          name = "(&${ldapFilter}(|(mail=?)(uid=?)))";
          email = "(&${ldapFilter}(mail=?))";
        };
        attributes = {
          name = "mail";
          email = "mail";
          description = "cn";
        };
      };

      session.rcpt = {
        directory = "'ldap'";
        rewrite = [
          {
            "if" = "key_exists('aliases', rcpt)";
            "then" = "key_get('aliases', rcpt)";
          }
          {
            "if" = "matches('^joerg[.+][^@]+@thalheim\\.io$', rcpt)";
            "then" = "'joerg@thalheim.io'";
          }
          {
            "if" = "matches('^shannan[.+][^@]+@lekwati\\.com$', rcpt)";
            "then" = "'shannan@lekwati.com'";
          }
          {
            "if" = "matches('^devkid-[^@.]+@devkid\\.net$', rcpt)";
            "then" = "'devkid@devkid.net'";
          }
          {
            "if" = "matches('^(info|dave)\\.[^@.]+@davhau\\.com$', rcpt)";
            "then" = "'info@davhau.com'";
          }
          {
            "if" = "rcpt == 'hello@handi-work.co.uk'";
            "then" = "'shannan@lekwati.com'";
          }
          {
            "if" = "matches('^ls1-logins-[^@.]+@thalheim\\.io$', rcpt)";
            "then" = "'ls1-logins@lists.lrz.de'";
          }
          { "else" = false; }
        ];
        # rcpt/rcpt_domain are post-rewrite here
        relay = [
          {
            "if" = "!is_empty(authenticated_as)";
            "then" = true;
          }
          {
            "if" = "rcpt_domain == 'lists.lrz.de' || key_exists('alias-targets', rcpt)";
            "then" = true;
          }
          { "else" = false; }
        ];
      };
      session.data.limits.size = 51200000;

      queue.connection.default = {
        ehlo-hostname = "mail.thalheim.io";
        source-ips = [
          config.networking.eve.ipv4.address
          config.networking.eve.ipv6.address
        ];
      };
      queue.tls.default = {
        dane = "optional";
        starttls = "optional";
        allow-invalid-certs = false;
      };

      auth.dkim.sign = [
        {
          "if" =
            "is_empty(authenticated_as) && local_port == 25 || !("
            + lib.concatMapStringsSep " || " (d: "sender_domain == '${d}'") dkimDomains
            + ")";
          "then" = false;
        }
        { "else" = "['rsa-' + sender_domain, 'ed25519-' + sender_domain]"; }
      ];
      auth.arc.seal = "'ed25519-thalheim.io'";
      signature = lib.listToAttrs (
        lib.concatMap (
          domain:
          map
            (
              algo:
              lib.nameValuePair "${algo}-${domain}" {
                private-key = "%{file:/run/credentials/stalwart.service/dkim-${domain}.${algo}}%";
                inherit domain;
                selector = "${dkimSelector}${lib.substring 0 1 algo}";
                headers = [
                  "From"
                  "To"
                  "Cc"
                  "Date"
                  "Subject"
                  "Message-ID"
                  "MIME-Version"
                  "Content-Type"
                  "In-Reply-To"
                  "References"
                  "List-Id"
                ];
                algorithm = "${algo}-sha256";
                canonicalization = "relaxed/relaxed";
                report = false;
              }
            )
            [
              "rsa"
              "ed25519"
            ]
        ) dkimDomains
      );

      report = {
        domain = "thalheim.io";
        submitter = "'mail.thalheim.io'";
        analysis.addresses = [
          "postmaster@*"
          "joerg.dmarc@thalheim.io"
          "joerc.dmarc@thalheim.io"
          "joerg.smtp-tls@thalheim.io"
        ];
        analysis.forward = true;
      };

      # folder names as migrated from dovecot
      email.folders = {
        junk = {
          name = "Spam";
          aliases = [
            "Junk"
            "Junk Mail"
          ];
        };
        trash.name = "Trash";
        sent.name = "Sent";
        drafts.name = "Drafts";
        archive.name = "Archive";
      };
    };
  };

  systemd.services.stalwart = {
    after = [
      "lldap.service"
      "acme-thalheim.io.service"
    ];
    wants = [ "acme-finished-thalheim.io.target" ];
    restartTriggers = [ staticConfig ];
    preStart = ''
      {
        cat ${staticConfig}
        printf '\n[lookup.aliases]\n'
        sed -E '/^[[:space:]]*(#|$)/d; s/^[[:space:]]*([^[:space:]]+)[[:space:]]+([^[:space:]]+).*$/"\1" = "\2"/' "$CREDENTIALS_DIRECTORY/aliases"
        printf '\n[lookup.alias-targets]\n'
        sed -E '/^[[:space:]]*(#|$)/d; s/^[[:space:]]*[^[:space:]]+[[:space:]]+([^[:space:]]+).*$/\1/' "$CREDENTIALS_DIRECTORY/aliases" \
          | grep -vE '@(${lib.concatMapStringsSep "|" lib.escapeRegex mailDomains})$' | sort -u \
          | sed -E 's/.*/"&" = ""/'
      } > ${runtimeConfig}
    '';
    serviceConfig = {
      RuntimeDirectory = "stalwart";
      RuntimeDirectoryMode = "0700";
      LoadCredential = [
        "aliases:${config.clan.core.vars.generators.postfix-aliases.files.virtual-aliases.path}"
      ];
      ExecStart = lib.mkForce [
        ""
        "${lib.getExe cfg.package} --config=${runtimeConfig}"
      ];
      SupplementaryGroups = [ "nginx" ];
    };
  };
  systemd.services.stalwart.unitConfig.ConditionPathExists = lib.mkForce [ ];

  # Local domains live in the internal store even with an LDAP directory.
  systemd.services.stalwart-domains = {
    wantedBy = [ "multi-user.target" ];
    after = [ "stalwart.service" ];
    requires = [ "stalwart.service" ];
    restartTriggers = [ (builtins.toJSON mailDomains) ];
    path = [
      pkgs.curl
      pkgs.jq
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      LoadCredential = [ "admin_password:${adminPasswordFile}" ];
      DynamicUser = true;
    };
    script = ''
      set -euo pipefail
      api() { curl -fsS --retry 10 --retry-connrefused -u "admin:$(cat "$CREDENTIALS_DIRECTORY/admin_password")" "$@"; }
      existing=$(api 'http://127.0.0.1:8642/api/principal?types=domain&limit=1000' | jq -r '.data.items[].name')
      for d in ${toString mailDomains}; do
        if ! grep -qxF "$d" <<<"$existing"; then
          echo "creating domain $d"
          api -H 'Content-Type: application/json' -d "{\"type\":\"domain\",\"name\":\"$d\"}" http://127.0.0.1:8642/api/principal >/dev/null
        fi
      done
    '';
  };

  security.acme.certs."thalheim.io".reloadServices = [ "stalwart.service" ];

  networking.firewall.allowedTCPPorts = [
    25
    465
    587
    993
    4190
  ];

  services.nginx.virtualHosts =
    let
      stalwart = {
        proxyPass = "http://127.0.0.1:8642";
        proxyWebsockets = true;
        extraConfig = ''
          client_max_body_size 256M;
          proxy_read_timeout 1h;
        '';
      };
    in
    {
      "jmap.thalheim.io" = {
        useACMEHost = "thalheim.io";
        forceSSL = true;
        locations."/" = stalwart;
      };
      # bulwark owns / on mail.thalheim.io, stalwart the discovery/protocol paths
      "mail.thalheim.io".locations = {
        "/.well-known/" = stalwart;
        "/jmap" = stalwart;
        "/dav" = stalwart;
        "= /mail/config-v1.1.xml" = stalwart;
        "/autodiscover/" = stalwart;
      };
    }
    // lib.genAttrs (map (d: "autoconfig.${d}") dkimDomains) (_: {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".proxyPass = "http://127.0.0.1:8642";
    });

  clan.core.vars.generators.postfix-aliases = {
    files.virtual-aliases = { };
    prompts.aliases = {
      description = "virtual alias map (one 'alias destination' per line)";
      type = "multiline";
    };
    script = ''
      cp "$prompts"/aliases "$out"/virtual-aliases
    '';
  };

  # DNS: <selector>r._domainkey / <selector>e._domainkey TXT from the .txt files
  clan.core.vars.generators.stalwart-dkim = {
    files = lib.listToAttrs (
      lib.concatMap (d: [
        (lib.nameValuePair "${d}.rsa.key" { })
        (lib.nameValuePair "${d}.ed25519.key" { })
        (lib.nameValuePair "${d}.rsa.txt" { secret = false; })
        (lib.nameValuePair "${d}.ed25519.txt" { secret = false; })
      ]) dkimDomains
    );
    runtimeInputs = [
      pkgs.openssl
      pkgs.coreutils
    ];
    script = ''
      for d in ${toString dkimDomains}; do
        openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out "$out/$d.rsa.key"
        openssl genpkey -algorithm ed25519 -out "$out/$d.ed25519.key"
        printf 'v=DKIM1; k=rsa; p=%s' "$(openssl pkey -in "$out/$d.rsa.key" -pubout -outform DER | base64 -w0)" > "$out/$d.rsa.txt"
        printf 'v=DKIM1; k=ed25519; p=%s' "$(openssl pkey -in "$out/$d.ed25519.key" -pubout -outform DER | tail -c 32 | base64 -w0)" > "$out/$d.ed25519.txt"
      done
    '';
  };

  clan.core.vars.generators.stalwart = {
    files.ldap-password = { };
    files.admin-password = { };
    runtimeInputs = [ pkgs.openssl ];
    script = ''
      openssl rand -base64 32 | tr -d '\n' > "$out/ldap-password"
      openssl rand -base64 24 | tr -d '\n' > "$out/admin-password"
    '';
  };

  services.lldap.ensureGroups = [ "mail" ];
  services.lldap.ensureUsers.stalwart = {
    passwordFile = ldapPasswordFile;
    groups = [ "lldap_strict_readonly" ];
  };
}
