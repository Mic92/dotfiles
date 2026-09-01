# CI-triggered `flakelet update <name>` over ssh. nixbot effects get a
# short-lived ssh certificate from step-ca (OIDC provisioner "nixbot")
# whose principal is the token subject, e.g.
# repo:github:Mic92/nixbot:ref:refs/heads/main. Each principal may only
# enqueue the update of the services it is listed under.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.flakeletDeploy;
  unit = name: "flakelet-update@${name}.service";
  start = name: "/run/current-system/sw/bin/systemctl start --no-block ${unit name}";
in
{
  options.services.flakeletDeploy = {
    caPublicKey = lib.mkOption {
      type = lib.types.str;
      description = "SSH user CA that signs the deploy certificates.";
    };
    services = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options.principals = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            description = "Certificate principals allowed to update this flakelet.";
          };
        }
      );
      default = { };
    };
  };

  config = lib.mkIf (cfg.services != { }) {
    users.users.flakelet-deploy = {
      isSystemUser = true;
      group = "flakelet-deploy";
      shell = pkgs.bash;
    };
    users.groups.flakelet-deploy = { };

    # sshd StrictModes rejects symlinks into /nix/store
    environment.etc."ssh/flakelet-deploy-ca.pub" = {
      text = cfg.caPublicKey;
      mode = "0444";
    };
    environment.etc."ssh/flakelet-deploy-principals" = {
      text = lib.concatStrings (
        lib.concatLists (
          lib.mapAttrsToList (
            name: svc:
            map (p: ''
              command="${start name} && echo '${config.networking.hostName}: flakelet update of ${name} enqueued'" ${p}
            '') svc.principals
          ) cfg.services
        )
      );
      mode = "0444";
    };

    services.openssh.extraConfig = lib.mkAfter ''
      Match User flakelet-deploy
        TrustedUserCAKeys /etc/ssh/flakelet-deploy-ca.pub
        AuthorizedPrincipalsFile /etc/ssh/flakelet-deploy-principals
        AuthorizedKeysFile none
      Match all
    '';

    # The forced command pins the unit per principal; polkit only has to
    # let this user start the template at all.
    security.polkit.enable = true;
    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.systemd1.manage-units" &&
            subject.user == "flakelet-deploy" &&
            action.lookup("verb") == "start" &&
            action.lookup("unit").indexOf("flakelet-update@") == 0) {
          return polkit.Result.YES;
        }
      });
    '';

    systemd.services."flakelet-update@" = {
      description = "flakelet update of %i triggered by CI";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${lib.getExe config.services.flakelets.package} update %i";
      };
    };
  };
}
