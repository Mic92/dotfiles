{ nixos-generators
, pkgs
,
}:
# $ nix build .#nspawn-template --builders ''
# $ mkdir -p /var/lib/machines/joe02
# $ sudo tar -C /var/lib/machines/joe02 -xf ./result/tarball/nixos-system-x86_64-linux.tar.xz
# $ sudo unshare --uts --mount -- chroot /var/lib/machines/joe02 /sbin/init
nixos-generators.nixosGenerate {
  inherit pkgs;
  modules = [
    ({ config, ... }: {
      system.stateVersion = config.system.nixos.version;

      # include this in a normal profile
      #imports = [
      #  "${toString modulesPath}/virtualisation/lxc-container.nix"
      #];
      services.openssh = {
        enable = true;
        passwordAuthentication = false;
        settings.UseDns = false;
      };
      systemd.network.enable = true;
      networking.useDHCP = false;
      networking.useHostResolvConf = false;

      systemd.network.networks."50-container-host0.network".extraConfig = ''
        [Match]
        Virtualization = container
        Name = host0

        [Network]
        # increment this for new servers...
        Address = 2a01:4f8:10b:49f:1::3/80
        # ... and this
        Address = 192.168.21.3/24
        Gateway = 192.168.21.254
        LinkLocalAddressing = yes
        LLDP = yes
        EmitLLDP = customer-bridge
        IPv6AcceptRA = yes

        [DHCP]
        UseTimezone = yes
      '';

      users.users.root.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDW+YfsFtRz1h/0ubcKU+LyGfxH505yUkbWa5VtRFNWF2fjTAYGj6o5M4dt+fv1h370HXvvOBtt8sIlWQgMsD10+9mvjdXWhTcpnYPx4yWuyEERE1/1BhItrog6XJKAedbCDpQQ+POoewouiHWVAUfFByPj5RXuE8zKUeIEkGev/QKrKTLnTcS8zFs/yrokf1qYYR571B3U8IPDjpV/Y1GieG3MSNaefIMCwAAup1gPkUA0XZ4A1L7NdEiUEHlceKVu9eYiWUM+wDRunBXnLHubeGyP8KmBA7PNKgml3WWRNTZjqNQk4u9Bl+Qea5eCkD8KI257EqgXYXy0QBWNyF8X j03@l302"
      ];
    })
  ];
  format = "lxc";
}
