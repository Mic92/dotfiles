{ lib
, pkgs
, config
, inputs
, ...
}: {
  system.stateVersion = config.system.nixos.version;

  networking.firewall.enable = false;
  #boot.supportedFilesystems = [ "bcachefs" ];

  networking.nameservers = [
    # hurricane electric
    "74.82.42.42"
    "2001:470:20::2"
  ];

  networking.usePredictableInterfaceNames = false;
  systemd.network.enable = true;
  networking.useNetworkd = true;

  systemd.network.networks =
    lib.mapAttrs'
      (num: _:
        lib.nameValuePair "eth${num}" {
          matchConfig.Name = "eth${num}";
          networkConfig = {
            DHCP = "yes";
            LLMNR = true;
            IPv4LLRoute = true;
            LLDP = true;
            IPv6AcceptRA = true;
            # used to have a stable address for zfs send
            Address = "fd42:4492:6a6d:43:1::${num}/64";
          };
          dhcpConfig = {
            UseHostname = false;
            RouteMetric = 512;
          };
          ipv6AcceptRAConfig.Token = "::521a:c5ff:fefe:65d9";
        })
      {
        "0" = { };
        "1" = { };
        "2" = { };
        "3" = { };
      };

  imports = [
    ../modules/sshd/tor.nix
    ../modules/nix-daemon.nix
  ];

  documentation.enable = false;
  documentation.nixos.options.warningsAreErrors = false;

  # no auto-updates
  systemd.services.update-prefetch.enable = false;

  environment.systemPackages = with pkgs; [
    diskrsync
    partclone
    ntfsprogs
    ntfs3g
  ];

  systemd.services.hidden-ssh-announce = {
    description = "irc announce hidden ssh";
    after = [ "tor.service" "network.target" ];
    wants = [ "tor.service" "network.target" ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      set -efu
      until test -e /var/lib/tor/onion/ssh/hostname; do
      echo "still waiting for /var/lib/tor/onion/ssh/hostname"
      sleep 1
      done
      echo "SSH Hidden Service at $(cat /var/lib/tor/onion/ssh/hostname)" | \
        ${inputs.nur-packages.packages.${pkgs.hostPlatform.system}.ircsink}/bin/ircsink \
        --port=6697 --secure --server=irc.hackint.org --nick=nixos-installer --target="#krebs-announce"

    '';
    serviceConfig = {
      PrivateTmp = "true";
      User = "tor";
      Type = "oneshot";
    };
  };

  systemd.services.sshd.wantedBy = lib.mkForce [ "multi-user.target" ];

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDITBcN9iw5Fn7yyfgiWFet3QWDoMcUNtzLi+PNoYS7jksvcKZy5pLOjE6wCpkbYx+Tcb4MyvoWPXvwdo5FfL4XdhZRO+JlZ66p/rGssq/wEr2BBUwohP7o39JLtiyXGXSsK6MO2aceOFLQr4KAdaeD8ST0XumGcV6bGqIbjFsK5FCxFhO8NkCFtavBjDwKUm3uyOnVCWMp12abUphzxrVtWhcsnw5GapohATP03mCNxmrn/L7x393HutxgjyduScX7++MjwVE6J7wCnztPUtJbh9jYemr/K9fBMBbLhQagOjrlQYGU5frgmLrPCRZusyg5HjWx6gJIxs/DskfgmW+V
"
  ];
}
