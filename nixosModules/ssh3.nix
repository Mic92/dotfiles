{
  config,
  lib,
  pkgs,
  self,
  ...
}:

let
  cfg = config.services.ssh3;
in
{
  options.services.ssh3 = {
    enable = lib.mkEnableOption "SSH3 server";

    port = lib.mkOption {
      type = lib.types.port;
      default = 444;
      description = "Port on which to listen for SSH3 connections";
    };

    bindAddress = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "Address to bind the SSH3 server to";
    };

    urlPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "my-secret-path";
      description = ''
        Secret URL path to hide the SSH3 server behind.
        When set, the server will only respond to requests to this path.
      '';
    };

    useACMEHost = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "example.com";
      description = ''
        Use ACME certificates from the specified host.
        The host must be configured in security.acme.certs.
      '';
    };

    certificatePath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Path to the X.509 certificate file (overrides useACMEHost)";
    };

    keyPath = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Path to the X.509 private key file (overrides useACMEHost)";
    };

    authorizedKeys = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to authorized_keys file (defaults to ~/.ssh/authorized_keys)";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra arguments to pass to ssh3-server";
    };
  };

  config = lib.mkIf cfg.enable (
    let
      certPath =
        if cfg.certificatePath != null then
          cfg.certificatePath
        else if cfg.useACMEHost != null then
          "${config.security.acme.certs.${cfg.useACMEHost}.directory}/fullchain.pem"
        else
          null;

      keyPath =
        if cfg.keyPath != null then
          cfg.keyPath
        else if cfg.useACMEHost != null then
          "${config.security.acme.certs.${cfg.useACMEHost}.directory}/key.pem"
        else
          null;

      hasCertificate = certPath != null && keyPath != null;
    in
    {
      systemd.services.ssh3 = {
        description = "SSH3 Server";
        wantedBy = [ "multi-user.target" ];
        after = [
          "network.target"
        ]
        ++ lib.optional (cfg.useACMEHost != null) "acme-${cfg.useACMEHost}.service";
        wants = lib.optional (cfg.useACMEHost != null) "acme-${cfg.useACMEHost}.service";

        serviceConfig = {
          Type = "simple";
          ExecStart = lib.concatStringsSep " " (
            [
              "${self.packages.${pkgs.system}.ssh3}/bin/ssh3-server"
              "-bind"
              "${cfg.bindAddress}:${toString cfg.port}"
            ]
            ++ lib.optional (cfg.urlPath != null) "-url-path ${cfg.urlPath}"
            ++ lib.optional hasCertificate "-cert ${certPath}"
            ++ lib.optional hasCertificate "-key ${keyPath}"
            ++ lib.optional (cfg.authorizedKeys != null) "-authorized-keys ${cfg.authorizedKeys}"
            ++ cfg.extraArgs
          );

          Restart = "always";
          RestartSec = "10s";

          # Security hardening
          DynamicUser = false;
          User = "root"; # SSH3 needs root for PTY allocation
          Group = lib.mkIf (cfg.useACMEHost != null) config.security.acme.certs.${cfg.useACMEHost}.group;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = false; # Need access to home directories
          NoNewPrivileges = false; # Need for PTY allocation
          ReadWritePaths = [ "/var/log" ];
          StateDirectory = "ssh3";
          RuntimeDirectory = "ssh3";
        };
      };

      networking.firewall.allowedUDPPorts = [ cfg.port ];
      networking.firewall.allowedTCPPorts = [ cfg.port ];
    }
  );
}
