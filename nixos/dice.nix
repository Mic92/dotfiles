# NixOS Module for University of Edinburgh (DICE substitution)
# Support for kerberos, AFS and openvpn and ssh
{ pkgs, ... }:
{
  # AFS filesystem: http://computing.help.inf.ed.ac.uk/informatics-filesystem
  # - also requires kerberos
  # - mount with the `aklog -d` command
  services.openafsClient = {
    enable = true;
    cellName = "inf.ed.ac.uk";
  };

  environment.systemPackages = [
    pkgs.krb5Full
    # ssh with kerberos authentication
    (pkgs.openssh.override { withKerberos = true; withGssapiPatches = true; })

    # http://computing.help.inf.ed.ac.uk/openvpn
    (pkgs.writeScriptBin "openvpn-edinburgh" ''
       exec "${pkgs.openvpn}/bin/openvpn" ${./uni-edinburgh.ovpn}
     '')
  ];

  programs.ssh.extraConfig = ''
    Host *.inf.ed.ac.uk
       # optional: add your student id and DICE password
       #User s16916xx
       GSSAPIAuthentication yes
       GSSAPIDelegateCredentials yes
  '';

  # http://computing.help.inf.ed.ac.uk/TAGS/kerberos
  # copied from login server: bruegel.inf.ed.ac.uk
  # /etc/krb5.conf
  # test using kinit and your DICE password
  # $ kinit s16916XX
  environment.etc."krb5.conf".text = ''
    [libdefaults]
      ticket_lifetime = 64800
      dns_lookup_realm = true
      default_realm = INF.ED.AC.UK
      forwardable = true
      dns_lookup_kdc = true
    [realms]
      INF.ED.AC.UK = {
        admin_server = kdc.inf.ed.ac.uk:749
        default_domain = inf.ed.ac.uk
      }
    [domain_realm]
      inf.ed.ac.uk  =  INF.ED.AC.UK
      .inf.ed.ac.uk  =  INF.ED.AC.UK
    [capaths]
      INF.ED.AC.UK = {
            ED.AC.UK = EASE.ED.AC.UK
      }
      ED.AC.UK = {
            INF.ED.AC.UK = EASE.ED.AC.UK
      }
    [appdefaults]
      pam = {
        debug = false
        ticket_lifetime = 64800
        renew_lifetime = 64800
        forwardable = true
        krb4_convert = false
      }
      wallet = {
        wallet_server = wallet.inf.ed.ac.uk
      }
  '';

  # every researcher also get 500 GB storage
  fileSystems."/mnt/backup" = let
    uun = "s1691654"; # EASE ID
  in {
      device = "//csce.datastore.ed.ac.uk/csce/inf/users/${uun}";
      fsType = "cifs";
      options = let
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
        # cat > smb-secrets <<EOF
        # username=s16916XX
        # domain=ED
        # password=<EASE_PASSWORD>
        # EOF
      in ["${automount_opts},credentials=/home/joerg/git/nixos-configuration/secrets/smb-secrets"];
  };
}
