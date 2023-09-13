{ config, pkgs, ... }:
{
  systemd.services.matrix-hook = {
    description = "Matrix Hook";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      HTTP_ADDRESS = "[::1]";
      HTTP_PORT = "9088";
      MX_HOMESERVER = "https://matrix.thalheim.io";
      MX_ID = "@nix-community-bot:thalheim.io";
      MX_ROOMID = "!cBybDCkeRlSWfuaFvn:numtide.com";
      MX_MSG_TEMPLATE = "${pkgs.matrix-hook}/message.html.tmpl";
    };
    serviceConfig = {
      EnvironmentFile = [
        # format: MX_TOKEN=<token>
        config.sops.secrets.nix-community-bot-access-token.path
      ];
      Type = "simple";
      ExecStart = "${pkgs.matrix-hook}/bin/matrix-hook";
      Restart = "always";
      RestartSec = "10";
      DynamicUser = true;
      User = "matrix-hook";
      Group = "matrix-hook";
    };
  };
}
