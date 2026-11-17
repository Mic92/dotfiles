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
  credential = name: variant "File" { filePath = "/run/credentials/stalwart.service/${name}"; };
  apiUrl = "http://127.0.0.1:8642";

  acmeDir = config.security.acme.certs."thalheim.io".directory;

  variant = type: value: { "@type" = type; } // value;
  expr = match: default: {
    inherit match;
    "else" = default;
  };
  id = lib.replaceStrings [ "." ] [ "-" ];
in
{
  services.stalwart = {
    enable = true;
    package = pkgs.stalwart_1_0;
    stateVersion = "26.05";
    url = "https://jmap.thalheim.io";
    # Also accepted as `user%admin` master login for any account.
    admin = {
      enable = true;
      username = "admin";
    };
    credentials = {
      ldap_password = ldapPasswordFile;
      admin = adminPasswordFile;
    }
    // lib.listToAttrs (
      lib.concatMap (d: [
        (lib.nameValuePair "dkim-${d}.rsa" (dkimKey d "rsa"))
        (lib.nameValuePair "dkim-${d}.ed25519" (dkimKey d "ed25519"))
      ]) dkimDomains
    );

    provision = {
      enable = true;
      urls = [ apiUrl ];

      singletons = {
        SystemSettings = {
          defaultHostname = "mail.thalheim.io";
          defaultDomainId = "#domain-thalheim-io";
          defaultCertificateId = "#cert-wildcard";
        };
        Authentication.directoryId = "#ldap";
        Http = {
          clientIpSource = "xForwardedFor";
          # bulwark on mail.thalheim.io talks to jmap.thalheim.io from the browser
          usePermissiveCors = true;
        };

        MtaStageRcpt = {
          rewrite = expr [
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
              "then" = "'mlekwati@gmail.com'";
            }
            {
              "if" = "matches('^ls1-logins-[^@.]+@thalheim\\.io$', rcpt)";
              "then" = "'ls1-logins@lists.lrz.de'";
            }
          ] "false";
          # rcpt/rcpt_domain are post-rewrite here
          allowRelaying = expr [
            {
              "if" = "!is_empty(authenticated_as)";
              "then" = "true";
            }
            {
              "if" =
                "rcpt_domain == 'lists.lrz.de' || rcpt == 'mlekwati@gmail.com' || key_exists('alias-targets', rcpt)";
              "then" = "true";
            }
          ] "false";
        };
        MtaStageData.maxMessageSize = expr [ ] "51200000";

        ReportSettings = {
          outboundReportDomain = "thalheim.io";
          outboundReportSubmitter = expr [ ] "'mail.thalheim.io'";
          inboundReportAddresses = [
            "postmaster@*"
            "joerg.dmarc@thalheim.io"
            "joerc.dmarc@thalheim.io"
            "joerg.smtp-tls@thalheim.io"
          ];
          inboundReportForwarding = true;
        };

        # folder names as migrated from dovecot
        Email.defaultFolders =
          lib.genAttrs
            [
              "trash"
              "sent"
              "drafts"
              "archive"
            ]
            (use: {
              name = lib.toSentenceCase use;
              create = true;
              subscribe = true;
            })
          // {
            junk = {
              name = "Spam";
              aliases = [
                "Junk"
                "Junk Mail"
              ];
              create = true;
              subscribe = true;
            };
          };

        # Stalwart would otherwise download the rules from GitHub.
        SpamSettings.spamFilterRulesUrl = "file://${cfg.package.spam-filter}/spam-filter-rules.json.gz";
      };

      objects = {
        NetworkListener = {
          reconcile = true;
          match = [ "name" ];
          objects =
            let
              listener = protocol: bind: implicitTls: name: {
                inherit name protocol implicitTls;
                bind = [ bind ];
              };
            in
            lib.mapAttrs (name: mk: mk name) {
              smtp = listener "smtp" "[::]:25" false;
              submissions = listener "smtp" "[::]:465" true;
              submission = listener "smtp" "[::]:587" false;
              imap = listener "imap" "[::]:143" false;
              imaps = listener "imap" "[::]:993" true;
              sieve = listener "manageSieve" "[::]:4190" false;
              http = listener "http" "127.0.0.1:8642" false;
            };
        };

        Tracer = {
          reconcile = true;
          match = [ "@type" ];
          objects.stdout = variant "Stdout" {
            enable = true;
            level = "debug";
            ansi = false;
          };
        };

        # Stalwart would otherwise download the web UI from GitHub.
        Application = {
          reconcile = true;
          match = [ "description" ];
          objects.webui = {
            description = "Stalwart Web Interface";
            enable = true;
            resourceUrl = "file://${cfg.package.webui}/webui.zip";
            urlPrefix = [
              "/admin"
              "/account"
            ];
          };
        };

        Certificate = {
          reconcile = true;
          match = [ "certificate" ];
          objects.cert-wildcard = {
            certificate = variant "File" { filePath = "${acmeDir}/fullchain.pem"; };
            privateKey = variant "File" { filePath = "${acmeDir}/key.pem"; };
          };
        };

        Directory = {
          reconcile = true;
          match = [ "description" ];
          objects.ldap = variant "Ldap" {
            description = "lldap";
            url = "ldap://127.0.0.1:3890";
            baseDn = "ou=people,dc=eve";
            bindDn = "uid=stalwart,ou=people,dc=eve";
            bindSecret = credential "ldap_password";
            # lldap exposes no hashes, so look up the DN and bind as it
            bindAuthentication = true;
            timeout = 15000;
            filterLogin = "(&${ldapFilter}(mail=?))";
            filterRecipient = "(&${ldapFilter}(mail=?))";
            # LDAP groups are only used to gate access, not as mail groups.
            filterMemberOf = null;
            attrMemberOf = [ ];
            attrEmail = [ "mail" ];
            attrEmailAlias = [ ];
            attrDescription = [ "cn" ];
          };
        };

        Domain = {
          match = [ "name" ];
          objects = lib.listToAttrs (
            map (
              name:
              lib.nameValuePair "domain-${id name}" {
                inherit name;
                certificateManagement = variant "Manual" { };
                dnsManagement = variant "Manual" { };
                dkimManagement = variant "Manual" { };
              }
            ) mailDomains
          );
        };

        DkimSignature = {
          reconcile = true;
          match = [
            "@type"
            "domainId"
            "selector"
          ];
          objects = lib.listToAttrs (
            lib.concatMap (
              domain:
              lib.concatMap
                (
                  algo:
                  let
                    common = {
                      domainId = "#domain-${id domain}";
                      selector = "${dkimSelector}${lib.substring 0 1 algo.name}";
                      privateKey = credential "dkim-${domain}.${algo.name}";
                      stage = "active";
                    };
                  in
                  [
                    (lib.nameValuePair "dkim1-${algo.name}-${id domain}" (
                      variant "Dkim1${algo.type}" (
                        common
                        // {
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
                          canonicalization = "relaxed/relaxed";
                          report = false;
                        }
                      )
                    ))
                    # DKIM2 publishes keys in the same DNS record format, so it
                    # shares key and selector with DKIM1.
                    (lib.nameValuePair "dkim2-${algo.name}-${id domain}" (variant "Dkim2${algo.type}" common))
                  ]
                )
                [
                  {
                    name = "rsa";
                    type = "RsaSha256";
                  }
                  {
                    name = "ed25519";
                    type = "Ed25519Sha256";
                  }
                ]
            ) dkimDomains
          );
        };

        MtaConnectionStrategy = {
          match = [ "name" ];
          objects.connection-default = {
            name = "default";
            ehloHostname = "mail.thalheim.io";
            localIps = [
              { localIp = config.networking.eve.ipv4.address; }
              { localIp = config.networking.eve.ipv6.address; }
            ];
          };
        };

        # Bulwark and Authelia connect from eve itself; failed logins they
        # relay must not get eve auto-banned.
        AllowedIp = {
          match = [ "address" ];
          objects = lib.listToAttrs (
            lib.imap0 (i: address: lib.nameValuePair "allowed-ip-${toString i}" { inherit address; }) [
              "127.0.0.1"
              "::1"
              config.networking.eve.ipv4.address
              config.networking.eve.ipv6.address
            ]
          );
        };
      };
    };
  };

  systemd.services.stalwart = {
    after = [
      "lldap.service"
      "acme-thalheim.io.service"
    ];
    wants = [ "acme-finished-thalheim.io.target" ];
    serviceConfig.SupplementaryGroups = [ "nginx" ];
  };

  # The alias map is private, so it is provisioned from the clan var at
  # runtime instead of going through the world-readable provision plan.
  systemd.services.stalwart-aliases = {
    wantedBy = [ "multi-user.target" ];
    partOf = [ "stalwart.service" ];
    after = [ "stalwart-provision.service" ];
    requires = [ "stalwart-provision.service" ];
    path = [
      pkgs.jq
      cfg.provision.package
    ];
    environment = {
      STALWART_URL = apiUrl;
      STALWART_USER = cfg.admin.username;
      # stalwart-cli caches the server schema and fails without a writable cache.
      XDG_CACHE_HOME = "/var/cache/stalwart-aliases";
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      DynamicUser = true;
      CacheDirectory = "stalwart-aliases";
      LoadCredential = [
        "admin:${adminPasswordFile}"
        "aliases:${config.clan.core.vars.generators.postfix-aliases.files.virtual-aliases.path}"
      ];
    };
    script = ''
      set -euo pipefail
      STALWART_PASSWORD=$(cat "$CREDENTIALS_DIRECTORY/admin")
      export STALWART_PASSWORD
      # Destinations outside our domains must be allowed for relaying.
      jq -Rnc --argjson local ${lib.escapeShellArg (builtins.toJSON mailDomains)} '
        [inputs | select(test("^\\s*(#|$)") | not) | [splits("\\s+")] | map(select(. != ""))
         | {key: .[0], value: .[1]}] as $aliases
        | ($aliases | map(.value) | unique
           | map(select((split("@")[1]) as $d | $local | index($d) | not))) as $targets
        | def entries(ns): to_entries | map({key: "\(ns)-\(.key)", value: (.value + {namespace: ns})}) | from_entries;
          {"@type": "reconcile", object: "MemoryLookupKeyValue", matchOn: ["namespace", "key"],
           scope: {namespace: "aliases"}, value: ($aliases | entries("aliases"))},
          {"@type": "reconcile", object: "MemoryLookupKey", matchOn: ["namespace", "key"],
           scope: {namespace: "alias-targets"}, value: ($targets | map({key: .}) | entries("alias-targets"))},
          {"@type": "create", object: "Action", value: {reload: {"@type": "ReloadLookupStores"}}}
      ' "$CREDENTIALS_DIRECTORY/aliases" | stalwart-cli apply --stdin --quiet
    '';
  };

  security.acme.certs."thalheim.io".reloadServices = [ "stalwart.service" ];

  networking.firewall.allowedTCPPorts = [
    25
    143
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
        locations."/login" = stalwart // {
          extraConfig = stalwart.extraConfig + "limit_req zone=login burst=10 nodelay;";
        };
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
      locations = {
        "= /mail/config-v1.1.xml".proxyPass = "http://127.0.0.1:8642";
        "/.well-known/".proxyPass = "http://127.0.0.1:8642";
        "/autodiscover/".proxyPass = "http://127.0.0.1:8642";
        "/".return = "404";
      };
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
