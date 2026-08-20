{ config, pkgs, ... }:
let
  inherit (config.services.matrix-synapse.settings) server_name;
  nginx-vhost = "matrix.thalheim.io";
  element-web-thalheim.io =
    pkgs.runCommand "element-web-with-config" { nativeBuildInputs = [ pkgs.buildPackages.jq ]; }
      ''
        cp -r ${pkgs.element-web} $out
        chmod -R u+w $out
        jq '."default_server_config"."m.homeserver" = { "base_url": "https://${nginx-vhost}:443", "server_name": "${server_name}" }' \
          > $out/config.json < ${pkgs.element-web}/config.json
        ln -s $out/config.json $out/config.${nginx-vhost}.json
      '';
in
{
  services.matrix-synapse = {
    enable = true;
    settings = {
      server_name = "thalheim.io";
      public_baseurl = "https://${nginx-vhost}";
      enable_registration = false;
      dynamic_thumbnails = true;
      max_upload_size = "30M";
      listeners = [
        {
          port = 8043;
          bind_addresses = [ "127.0.0.1" ];
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            {
              names = [
                "client"
                "federation"
              ];
              compress = false;
            }
          ];
        }
      ];
      trusted_key_servers = [ { server_name = "matrix.org"; } ];
    };
  };

  systemd.services.matrix-synapse.after = [ "postgresql.service" ];
  services.postgresql.ensureUsers = [ { name = "matrix-synapse"; } ];

  services.nginx.virtualHosts.${nginx-vhost} = {
    forceSSL = true;
    useACMEHost = "thalheim.io";
    extraConfig = ''
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_read_timeout 600;
      client_max_body_size 30M;
    '';
    locations."/_matrix".proxyPass = "http://127.0.0.1:8043";
    locations."/_synapse/client".proxyPass = "http://127.0.0.1:8043";

    locations."/".root = element-web-thalheim.io;
  };
}
