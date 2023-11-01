{ config, pkgs, lib, ... }:

let
  cfg = config.services.ksmbd;

  smbToString = x:
    if builtins.typeOf x == "bool"
    then lib.boolToString x
    else toString x;

  shareConfig = name:
    let share = lib.getAttr name cfg.shares; in
    "[${name}]\n " + (smbToString (
      map
        (key: "${key} = ${smbToString (lib.getAttr key share)}\n")
        (lib.attrNames share)
    ));
in
{
  options = {
    services.ksmbd = {
      enable = lib.mkEnableOption "Enable cifsd kernel server";

      openFirewall = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = lib.mdDoc ''
          Whether to automatically open the necessary ports in the firewall.
        '';
      };

      securityType = lib.mkOption {
        type = lib.types.str;
        default = "user";
        description = "Samba security type";
      };

      extraConfig = lib.mkOption {
        type = lib.types.lines;
        default = "";
        description = ''
          Additional global section and extra section lines go in here.
        '';
        example = ''
          guest account = nobody
          map to guest = bad user
        '';
      };

      shares = lib.mkOption {
        default = { };
        description = ''
          A set describing shared resources.
          See <command>man smb.conf</command> for options.
        '';
        type = lib.types.attrsOf (lib.types.attrsOf lib.types.unspecified);
        example = lib.literalExample ''
          { public =
            { path = "/srv/public";
              "read only" = true;
              browseable = "yes";
              "guest ok" = "yes";
              comment = "Public samba share.";
            };
          }
        '';
      };

      users = lib.mkOption {
        default = [ ];
        type = lib.types.listOf (lib.types.submodule {
          options = {
            user = lib.mkOption {
              type = lib.types.str;
            };
            passwordFile = lib.mkOption {
              type = lib.types.path;
            };
          };
        });
      };
    };
  };
  config = lib.mkIf cfg.enable {
    boot.kernelModules = [ "ksmbd" ];
    environment.systemPackages = [ pkgs.ksmbd-tools ];

    environment.etc."ksmbd/ksmbd.conf".text = ''
      [global]
      security = ${cfg.securityType}
      ${cfg.extraConfig}

      ${smbToString (map shareConfig (lib.attrNames cfg.shares))}
    '';

    systemd.services.ksmbd = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      path = [ pkgs.ksmbd-tools ];
      preStart = builtins.concatStringsSep "\n"
        (map (it: "ksmbd.adduser -i /run/ksmbd/passwd -a ${it.user} < ${it.passwordFile}") cfg.users);
      serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.ksmbd-tools}/bin/ksmbd.mountd -C /etc/ksmbd/ksmbd.conf -P /run/ksmbd/passwd";
        Restart = "always";
        PrivateTmp = true;
        RuntimeDirectory = "ksmbd";
      };
    };

    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ 445 ];
  };
}
