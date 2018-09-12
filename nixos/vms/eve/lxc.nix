{ config, lib, pkgs, ... }:

with builtins;
let

  nft_rules = pkgs.callPackage (import ./nft.nix) {};

  network = (import ./network.nix) {inherit lib;};
  fuidshift = pkgs.callPackage (import ./fuidshift.nix) {};

  lxcRuby = (import ./lxc/default.nix);

  rubyScript = script: pkgs.writeScript (baseNameOf (toString script)) ''
    #!${pkgs.stdenv.shell}
    ${lxcRuby.wrappedRuby}/bin/ruby ${script}
  '';

  lxcConfig = container:
    pkgs.writeText "lxc-config" (
    with container; ''
      lxc.uts.name = ${name}
      lxc.rootfs.path = /lxc/${name}/rootfs

      lxc.net.0.type = veth
      lxc.net.0.link = ${network.bridge}
      lxc.net.0.flags = up
      lxc.net.0.name = eth0
      lxc.net.0.mtu = 1500
      lxc.net.0.ipv4.gateway = 172.23.75.2
      lxc.net.0.ipv6.gateway = fe80::1
      lxc.net.0.ipv4.address = ${ipv4}/26
      lxc.net.0.ipv6.address = ${ipv6}/80
      lxc.net.0.ipv6.address = ${ula}/80
      lxc.net.0.veth.pair = ${substring 0 15 "lxc_${name}"}
      lxc.autodev = 1
      lxc.pty.max = 100

      lxc.cap.keep = chown dac_override dac_read_search fowner fsetid ipc_owner kill lease linux_immutable net_bind_service net_broadcast net_raw setgid setfcap setpcap setuid sys_admin sys_chroot sys_nice sys_ptrace sys_tty_config sys_resource sys_boot audit_write audit_control mknod

      lxc.init.cmd = /usr/lib/systemd/systemd systemd.legacy_systemd_cgroup_controller=1

      # Set the halt/stop signals
      lxc.signal.halt=SIGRTMIN+4
      lxc.signal.stop=SIGRTMIN+14

      lxc.cgroup.memory.soft_limit_in_bytes = 1500M
      lxc.cgroup.memory.limit_in_bytes = 2000M
      lxc.cgroup.cpu.shares = 256
      lxc.cgroup.blkio.weight = 500

      lxc.idmap = u 0 100000 65536
      lxc.idmap = g 0 100000 65536

      # Blacklist some syscalls which are not safe in privileged
      # containers
      lxc.seccomp.profile = /etc/nixos/lxc/default.seccomp

      lxc.include = ${pkgs.lxcfs}/share/lxc/config/common.conf.d/00-lxcfs.conf
      lxc.include = ${pkgs.lxc}/share/lxc/config/userns.conf

      # Setup the default mounts
      lxc.mount.auto = cgroup:mixed proc:mixed sys:mixed
      lxc.mount.entry = /sys/fs/fuse/connections sys/fs/fuse/connections none bind,optional 0 0
      lxc.mount.entry = /data/pacman/pkg var/cache/pacman/pkg none bind 0 0
      lxc.mount.entry = /data/pacman/sync var/lib/pacman/sync none bind 0 0
      lxc.mount.entry = /lxc/login/rootfs/var/lib/aurrepo srv/repo none bind,ro,create=dir,umask=0 0 0
      lxc.mount.entry = tmpfs dev/shm tmpfs nosuid,nodev,mode=1777,create=dir 0 0
      lxc.mount.entry = tmpfs run tmpfs nosuid,nodev,noexec,mode=0755,size=128m 0 0

      lxc.hook.clone = ${rubyScript "/etc/nixos/lxc/hooks/cleanup-lxc-config"}
      lxc.hook.clone = ${rubyScript "/etc/nixos/lxc/hooks/create-lxc-config"}
      lxc.hook.clone = /etc/nixos/lxc/hooks/setup-machine-id
      lxc.hook.clone = ${rubyScript "/etc/nixos/lxc/hooks/update-zone"}
      lxc.hook.clone = /etc/nixos/lxc/hooks/create-systemd-service

      lxc.hook.autodev = /etc/nixos/lxc/hooks/dn42-routes

      lxc.apparmor.allow_incomplete = 1
      lxc.apparmor.profile = lxc-container-default-with-nesting
      lxc.cgroup.pattern = ""

      lxc.include = /lxc/${name}/local.conf
    '');

  lxcService = container: with container; {
    description = "LXC - ${name}";
    after = ["network.target"];
    requires = ["nftables.service"];
    wantedBy = ["multi-user.target"];
    path = with pkgs; [ iproute utillinux bash procps ];
    restartIfChanged = false;
    enable = enabled;
    preStart = ''
      mkdir -p /var/lib/lxc/rootfs
      touch /lxc/${name}/local.conf
      ln -sf ${lxcConfig container} /lxc/${name}/config
      #[[ "$(stat -c '%u' /lxc/${name}/rootfs)" = 10000 ]] || ${fuidshift}/bin/fuidshift /lxc/${name}/rootfs b:0:100000:65536
    '';
    serviceConfig = {
      ExecStart="${pkgs.lxc}/bin/lxc-start -n ${name} -F";
      ExecStop="${pkgs.lxc}/bin/lxc-stop -n ${name}";
      Restart="on-failure";
      RestartSec="2s";
      TasksMax="32768";
      # delegate to systemd container
      LimitNPROC="infinity";
      LimitNOFILE="1048576";
      Delegate="yes";
    };
  };

  lxcServices = lib.mapAttrs' (name: container: {
    name="lxc-${name}";
    value=(lxcService container);
  }) network.lxcContainers;

in {
  users.extraUsers.root = {
    subUidRanges = [ { startUid = 100000; count = 65536; } ];
    subGidRanges = [ { startGid = 100000; count = 65536; } ];
  };

  virtualisation.lxc = {
    enable = true;
    systemConfig = ''
      lxc.lxcpath = /lxc/
      lxc.bdev.zfs.root = zroot/lxc
      lxc.cgroup.pattern = lxc.slice/%n.service
    '';
    lxcfs.enable = true;
  };

  systemd.services = {
    nftables = {
      description="Cgroup management daemon";
      after = [ "local-fs.target" ];
      before = [ "network-pre.target" ];
      wants = [ "network-pre.target" ];
      wantedBy = [ "multi-user.target" ];
      reloadIfChanged = true;
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${nft_rules}";
        ExecStop = "${pkgs.nftables}/bin/nft flush ruleset";
        ExecReload = "${nft_rules}";
        KillMode = "process";
        RemainAfterExit = true;
      };
    };
  } // lxcServices;
}
