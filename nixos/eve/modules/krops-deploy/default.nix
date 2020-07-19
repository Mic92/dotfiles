{ config, pkgs, ... }: let
  krops-deploy-shell = pkgs.callPackage ./krops-deploy-shell.nix {};
  github-action-key = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILbo13TLoeGoXR1yMu4vwixQf29EbmUxE1LQc2X1/5fY github@github.com
  '';

  krops-deploy-key = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM3C8eER19KZZk8mKKrwpD4GUOVa0ct1YjGBkmDDUBav krops-deploy@eve.thalheim.io
  '';
in {
  users.extraUsers = {
    krops-deploy = {
      isSystemUser = true;
      createHome = true;

      home = "/var/lib/deploy";
      extraGroups = ["wheel"];
      shell = "/run/current-system/sw/bin/bash";
      openssh.authorizedKeys.keys = [
        ''command="${krops-deploy-shell}/bin/krops-deploy-shell",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${github-action-key}''
      ];
    };
    root.openssh.authorizedKeys.keys = [ krops-deploy-key ];
  };

  users.users.krops-deploy.extraGroups = [ "keys" ];
  sops.secrets.krops-deploy-ssh = {
    path = "${config.users.extraUsers.krops-deploy.home}/.ssh/id_ed25519";
    owner = "krops-deploy";
  };
}
