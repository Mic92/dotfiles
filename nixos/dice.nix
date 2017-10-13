# NixOS Module for University of Edinburgh (DICE substitution)
# Support for kerberos, AFS and openvpn and ssh
{ pkgs, config, ... }:

let
  uun = "s1691654"; # EASE ID
in {
  # AFS filesystem: http://computing.help.inf.ed.ac.uk/informatics-filesystem
  # - also requires kerberos
  # - mount with the `aklog -d` command
  services.openafsClient = {
    enable = false;
    cellName = "inf.ed.ac.uk";
    sparse = true;
  };

  environment.systemPackages = [
    # ssh with kerberos authentication
    (pkgs.openssh.override { withKerberos = true; withGssapiPatches = true; })

    # http://computing.help.inf.ed.ac.uk/openvpn
    (pkgs.writeScriptBin "openvpn-edinburgh" ''
       export PATH=$PATH:${pkgs.iproute}/bin
       exec "${pkgs.openvpn}/bin/openvpn" ${./uni-edinburgh.ovpn}
     '')

    # http://computing.help.inf.ed.ac.uk/openvpn
    (pkgs.writeScriptBin "openvpn-edinburgh-full" ''
       export PATH=$PATH:${pkgs.iproute}/bin
       exec "${pkgs.openvpn}/bin/openvpn" --redirect-gateway def1  ${./uni-edinburgh.ovpn}
     '')
  ];

  programs.ssh.extraConfig = ''
    Host *.inf.ed.ac.uk
       # optional: add your student id and DICE password
       #User s16916xx
       GSSAPIAuthentication yes
       GSSAPIDelegateCredentials yes
  '';

  # Password-less kerberos (https://kb.iu.edu/d/aumh)
  # $ ktutil
  #ktutil:  addent -password -p s16916XX -k 1 -e aes256-cts
  #ktutil:  wkt /etc/nixos/secrets/krb5.keytab
  #ktutil:  quit
  environment.etc."krb5.keytab".source = "/etc/nixos/secrets/krb5.keytab";

  # http://computing.help.inf.ed.ac.uk/TAGS/kerberos
  # copied from login server: bruegel.inf.ed.ac.uk
  # /etc/krb5.conf
  # test using kinit and your DICE password
  # $ kinit s16916XX -k
  krb5 = {
    enable = true;
    kerberos = pkgs.krb5Full;
    libdefaults = {
      ticket_lifetime = 64800;
      dns_lookup_realm = true;
      default_realm = "INF.ED.AC.UK";
      forwardable = true;
      dns_lookup_kdc = true;
    };
    realms."INF.ED.AC.UK" = {
      admin_server = "kdc.inf.ed.ac.uk:749";
      default_domain = "inf.ed.ac.uk";
    };
    domain_realm = {
      "inf.ed.ac.uk" = "INF.ED.AC.UK";
      ".inf.ed.ac.uk"= "INF.ED.AC.UK";
    };
    capaths = {
      "INF.ED.AC.UK" = {
        "ED.AC.UK" = "EASE.ED.AC.UK";
      };
      "ED.AC.UK" = {
        "INF.ED.AC.UK" = "EASE.ED.AC.UK";
      };
    };
    appdefaults = {
      pam = {
        debug = false;
        ticket_lifetime = 64800;
        renew_lifetime = 64800;
        forwardable = true;
        krb4_convert = false;
      };
      wallet = {
        wallet_server = "wallet.inf.ed.ac.uk";
      };
    };
  };

  systemd.timers.kinit = {
    wantedBy = ["multi-user.target"];
    timerConfig.OnBootSec="10min";
    timerConfig.OnUnitActiveSec="15min";
  };
  systemd.services.kinit = {
    # cifs mount from ./dice.nix
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = with pkgs; [
        "${krb5Full}/bin/kinit ${uun} -k"
        "${utillinux}/bin/runuser -u joerg -- ${krb5Full}/bin/kinit ${uun} -k"
        "${utillinux}/bin/runuser -u joerg -- ${config.boot.kernelPackages.openafsClient}/bin/aklog -d"
      ];
    };
  };

  # every researcher also get 500 GB storage
  fileSystems."/mnt/backup" = {
      device = "//csce.datastore.ed.ac.uk/csce/inf/users/${uun}";
      fsType = "cifs";
      options = let
        automount_opts = "noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=10s,x-systemd.mount-timeout=10s,soft";
        # cat > smb-secrets <<EOF
        # username=s16916XX
        # domain=ED
        # password=<EASE_PASSWORD>
        # EOF
      in ["${automount_opts},credentials=/home/joerg/git/nixos-configuration/secrets/smb-secrets"];
  };
}
