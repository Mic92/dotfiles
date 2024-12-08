{
  services.telegraf.extraConfig.inputs = {
    net_response =
      map
        (host: {
          protocol = "tcp";
          address = "${host}:22";
          send = "SSH-2.0-Telegraf";
          expect = "SSH-2.0";
          tags.host = host;
          tags.org = "dave";
          timeout = "10s";
        })
        [
          "[fc9f:379f:7135:b48d:1817:0000:0000:0001]"
          # "[fc9f:379f:71f5:8aeb:6e2b:0000:0000:0001]"  # offline for now
        ];
  };
}
