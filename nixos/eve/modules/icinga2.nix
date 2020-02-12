{ config, pkgs, lib, ... }:
let
  icingaConf = pkgs.writeText "eve.thalheim.io.conf" ''
    # TODO: https://www.jens.bruntt.dk/icinga2-sending-pushover-alerts-revisited/
    object HostGroup "nixos-servers" {
      display_name = "Mic92's Servers"
      assign where host.vars.os == "NixOS"
    }

    object ApiUser "mic92-api" {
      password = "@API_PASSWORD@";
      permissions = [ "objects/query/Host", "objects/query/Service" ]
    }

    object UserGroup "eveadmins" {
      display_name = "Eve Admin Group"
    }

    object User "mic92-email" {
      import "generic-user"

      display_name = "Icinga 2 Admin"
      groups = [ "eveadmins" ]

      email = "joerg@thalheim.io"
    }

    object User "mic92-pushover" {
      import "generic-user"

      display_name = "Icinga 2 Admin"
      groups = [ "eveadmins" ]

      email = "@PUSHOVER_EMAIL@"
    }

    object Host "eve.thalheim.io" {
      import "eve-host"
      address = "95.216.112.61"
      address6 = "2a01:4f9:2b:1605::1"
      vars.os = "NixOS"
      vars.noagent = true
      vars.notification["mail"] = {
        groups = [ "eveadmins" ]
      }
    }

    ## {{{ Templates

    template Service "eve-service" {
      max_check_attempts = 5
      check_interval = 1m
      retry_interval = 30s
      command_endpoint = "icingamaster.bsd.services"
    }

    ## prosody
    template Service "eve-tcp4-service" {
      import "eve-service"
      check_command = "tcp"
      vars.tcp_ipv4 = true;
    }

    template Service "eve-tcp6-service" {
      import "eve-service"
      check_command = "tcp"
      vars.tcp_ipv6 = true;
    }

    template Service "eve-http-service" {
      import "eve-service"
      check_command = "http"
      vars.http_sni = true
      vars.http_onredirect = "follow"
      vars.http_ssl_force_tlsv1_2_or_higher = true
    }

    template Service "eve-http4-service" {
      import "eve-http-service"
      vars.http_ipv4 = true
    }

    template Service "eve-http6-service" {
      import "eve-http-service"
      vars.http_ipv6 = true
    }

    template Service "eve-smtp-service" {
      import "eve-service"
      check_command = "smtp"
      vars.smtp_starttls = true
    }

    template Service "eve-smtp4-service" {
      import "eve-smtp-service"
      vars.smtp_ipv4 = true
    }

    template Service "eve-smtp6-service" {
      import "eve-smtp-service"
      vars.smtp_ipv6 = true
    }

    template Host "eve-host" {
      max_check_attempts = 3
      check_interval = 1m
      retry_interval = 30s
      check_command = "hostalive"
    }
    # }}}

    # {{{ Services
    ${config.services.icinga2.extraConfig}
    # }}}
  '';
in {
  options.services.icinga2.extraConfig = lib.mkOption {
    type = lib.types.lines;
    default = "";
    description = "Icinga configuration file";
  };

  config = {
    services.icinga2.extraConfig = ''
    '';
    systemd.services.icinga-sync = {
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ openssh gnused ];
      script = ''
        sed \
          -e "s!@API_PASSWORD@!$(</run/keys/icinga-api-password)!" \
          -e "s!@PUSHOVER_EMAIL@!$(</run/keys/icinga-pushover-email)!" \
          ${icingaConf} \
          > eve.thalheim.io.conf
        scp -o 'ProxyJump mic92@fw02.bsd.services' eve.thalheim.io.conf \
          mic92@icingamaster:eve.thalheim.io.conf
        ssh -J mic92@fw02.bsd.services mic92@icingamaster \
          'sudo service icinga2 reload || sudo icinga2 daemon -C'
      '';
      serviceConfig = {
        RuntimeDirectory = "icinga-sync";
        WorkingDirectory = "/run/icinga-sync/";
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };

    krops.secrets.files.icinga-api-password = {};
    krops.secrets.files.icinga-pushover-email = {};
  };
}
