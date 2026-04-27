{ pkgs, config, ... }:
let
  sieve-flagged-forward = pkgs.callPackage ../../pkgs/sieve-flagged-forward { };

  localConfig = pkgs.writeText "local.conf" ''
    logging {
      level = "notice";
    }
    classifier "bayes" {
      autolearn = true;
    }
    redis {
      servers = "/run/redis-rspamd/redis.sock";
    }
    dkim_signing {
      path = "/var/lib/rspamd/dkim/$domain.$selector.key";
      selector = "default";
      allow_username_mismatch = true;
    }
    arc {
      path = "/var/lib/rspamd/dkim/$domain.$selector.key";
      selector = "default";
      allow_username_mismatch = true;
    }
    milter_headers {
      use = ["authentication-results", "x-spam-status"];
      authenticated_headers = ["authentication-results"];
    }
    replies {
      action = "no action";
    }
    url_reputation {
      enabled = true;
    }
    phishing {
      openphish_enabled = true;
      # too much memory
      #phishtank_enabled = true;
    }
    neural {
      enabled = true;
    }
    neural_group {
      symbols = {
        "NEURAL_SPAM" {
          weight = 3.0; # sample weight
          description = "Neural network spam";
        }
        "NEURAL_HAM" {
          weight = -3.0; # sample weight
          description = "Neural network ham";
        }
      }
    }
  '';

  sieve-spam-filter = pkgs.callPackage ../../pkgs/sieve-spam-filter { };
in
{
  services.rspamd = {
    enable = true;
    extraConfig = ''
      .include(priority=1,duplicate=merge) "${localConfig}"
    '';

    postfix.enable = true;
    workers.controller = {
      extraConfig = ''
        count = 1;
        static_dir = "''${WWWDIR}";
        password = "$2$cifyu958qabanmtjyofmf5981posxie7$dz3taiiumir9ew5ordg8n1ia3eb73y1t55kzc9qsjdq1n8esmqqb";
        enable_password = "$2$cifyu958qabanmtjyofmf5981posxie7$dz3taiiumir9ew5ordg8n1ia3eb73y1t55kzc9qsjdq1n8esmqqb";
      '';
    };
  };

  services.dovecot2.includeFiles = [
    (pkgs.writeText "dovecot-rspamd.conf" ''
      sieve_plugins {
        sieve_imapsieve  = yes
        sieve_extprograms = yes
      }
      sieve_global_extensions {
        vnd.dovecot.pipe        = yes
        vnd.dovecot.environment = yes
      }
      sieve_pipe_bin_dir = ${
        pkgs.symlinkJoin {
          name = "sieve-pipe-bin";
          paths = [
            "${sieve-spam-filter}/bin"
            "${sieve-flagged-forward}/bin"
          ];
        }
      }

      namespace inbox {
        inbox = yes
        separator = .
        mailbox Spam {
          auto = subscribe
          special_use = \Junk

          # From elsewhere to Spam folder
          sieve_script report-spam {
            type  = before
            cause = copy
            path  = /var/lib/dovecot/sieve/report-spam.sieve
          }
        }
      }

      # From Spam folder to elsewhere
      imapsieve_from Spam {
        sieve_script report-ham {
          type  = before
          cause = copy
          path  = /var/lib/dovecot/sieve/report-ham.sieve
        }
      }

      # Notify Janet (opencrow) when a message is flagged/starred
      sieve_script report-flagged {
        type  = before
        cause = flag
        path  = /var/lib/dovecot/sieve/report-flagged.sieve
      }

      # Move Spam emails to Spam folder during LMTP delivery
      sieve_script move-to-spam {
        type = before
        path = /var/lib/dovecot/sieve/move-to-spam.sieve
      }
    '')
  ];

  services.nginx = {
    virtualHosts."rspamd.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        proxy_pass http://localhost:11334;
      '';
    };
  };

  systemd.services.rspamd.serviceConfig.SupplementaryGroups = [ "redis-rspamd" ];

  systemd.services.dovecot.preStart = ''
    mkdir -p /var/lib/dovecot/sieve/
    for i in ${sieve-spam-filter}/share/sieve-rspamd-filter/*.sieve \
             ${sieve-flagged-forward}/share/sieve-flagged-forward/*.sieve; do
      dest="/var/lib/dovecot/sieve/$(basename $i)"
      cp "$i" "$dest"
      ${config.services.dovecot2.package.passthru.dovecot_pigeonhole}/bin/sievec "$dest"
    done
    chown -R "${config.services.dovecot2.settings.mail_uid}:${config.services.dovecot2.settings.mail_gid}" /var/lib/dovecot/sieve
  '';
}
