# NixOS Module for University of Edinburgh (DICE substitution)
# Support for kerberos, AFS and openvpn and ssh
{ pkgs, config, ... }:

let
  uun = "s1691654"; # EASE ID
in
{
  environment.systemPackages = [
    # ssh with kerberos authentication
    #(pkgs.openssh.override { withKerberos = true; withGssapiPatches = true; })

    # http://computing.help.inf.ed.ac.uk/openvpn
    (pkgs.writeScriptBin "openvpn-edinburgh" ''
      export PATH=$PATH:${pkgs.iproute}/bin
      exec "${pkgs.openvpn}/bin/openvpn" ${./uni-edinburgh.ovpn}
    '')

    # http://computing.help.inf.ed.ac.uk/openvpn
    (pkgs.writeScriptBin "openvpn-edinburgh-full" ''
      export PATH=$PATH:${pkgs.iproute}/bin
      exec "${pkgs.openvpn}/bin/openvpn" --redirect-gateway def1 ipv6 --config ${./uni-edinburgh.ovpn}
    '')
  ];
  # every researcher also get 500 GB storage
  fileSystems."/mnt/backup" = {
    device = "//csce.datastore.ed.ac.uk/csce/inf/users/${uun}";
    fsType = "cifs";
    options =
      let
        automount_opts = "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s,soft,vers=2.0";
        # cat > smb-secrets <<EOF
        # username=s16916XX
        # domain=ED
        # password=<EASE_PASSWORD>
        # EOF
      in
      [ "${automount_opts},credentials=${config.sops.secrets.smb-secrets.path}" ];
  };

  sops.secrets.smb-secrets = { };
  sops.secrets.uni-vpn-auth = { };
}
