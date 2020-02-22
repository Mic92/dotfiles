{
  services.redis = {
    enable = true;
    port = 0;
    unixSocket = "/run/redis/redis.sock";
    extraConfig = ''
      unixsocketperm 660
    '';
  };
}
