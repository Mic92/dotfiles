{ ... }: {
  imports = [ ../../modules/tor-ssh.nix ];

  # for netdata
  services.tor.controlSocket.enable = true;

  environment.etc."netdata/python.d/tor.conf".text = ''
    local_socket:
      name: 'local'
      control_port: '/run/tor/control'
  '';
  users.users.netdata.extraGroups = [ "tor" ];
}
