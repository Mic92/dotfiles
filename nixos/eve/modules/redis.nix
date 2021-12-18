{
  services.redis.servers.rspamd = {
    enable = true;
    port = 0;
    unixSocket = "/run/redis-rspamd/redis.sock";
    unixSocketPerm = 660;
  };
}
