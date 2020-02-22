{
  services.redis = {
    enable = true;
    port = 0;
    unixSocket = "/run/redis/redis.sock";
    extraConfig = ''
      unixsocketperm 660
    '';
  };

  environment.etc."netdata/python.d/redis.conf".text = ''
    socket:
      name: 'local'
      socket: '/run/redis/redis.sock'
  '';

  users.users.netdata.extraGroups = [ "redis" ];
}
