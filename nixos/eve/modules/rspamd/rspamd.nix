{ pkgs
, config
, ...
}:
let
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

  services.dovecot2 = {
    mailboxes.Spam = {
      auto = "subscribe";
      specialUse = "Junk";
    };
    extraConfig = ''
      protocol imap {
        mail_plugins = $mail_plugins imap_sieve
      }

      plugin {
        sieve_plugins = sieve_imapsieve sieve_extprograms

        # From elsewhere to Spam folder
        imapsieve_mailbox1_name = Spam
        imapsieve_mailbox1_causes = COPY
        imapsieve_mailbox1_before = file:/var/lib/dovecot/sieve/report-spam.sieve

        # From Spam folder to elsewhere
        imapsieve_mailbox2_name = *
        imapsieve_mailbox2_from = Spam
        imapsieve_mailbox2_causes = COPY
        imapsieve_mailbox2_before = file:/var/lib/dovecot/sieve/report-ham.sieve

        # Move Spam emails to Spam folder
        sieve_before = /var/lib/dovecot/sieve/move-to-spam.sieve

        sieve_pipe_bin_dir = ${sieve-spam-filter}/bin
        sieve_global_extensions = +vnd.dovecot.pipe +vnd.dovecot.environment
      }
    '';
  };

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

  systemd.services.dovecot2.preStart = ''
    mkdir -p /var/lib/dovecot/sieve/
    for i in ${sieve-spam-filter}/share/sieve-rspamd-filter/*.sieve; do
      dest="/var/lib/dovecot/sieve/$(basename $i)"
      cp "$i" "$dest"
      ${pkgs.dovecot_pigeonhole}/bin/sievec "$dest"
    done
    chown -R "${config.services.dovecot2.mailUser}:${config.services.dovecot2.mailGroup}" /var/lib/dovecot/sieve
  '';
}
