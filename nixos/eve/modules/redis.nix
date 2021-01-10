{
  services.redis = {
    enable = true;
    port = 0;
    unixSocket = "/run/redis/redis.sock";
    settings.unixsocketperm = 660;
  };
}
