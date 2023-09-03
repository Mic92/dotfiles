{ config, pkgs, ... }:
let
  matrix-hook = pkgs.buildGoModule rec {
    pname = "matrix-hook";
    version = "unstable-2021-04-15";
    src = pkgs.fetchFromGitHub {
      owner = "pinpox";
      repo = "matrix-hook";
      rev = version;
      hash = "sha256-G5pq9sIz94V2uTYBcuHJsqD2/pMtxhWkAO8B0FncLbE=";
    };
    vendorHash = "sha256-185Wz9IpJRBmunl+KGj/iy37YeszbT3UYzyk9V994oQ=";
    postInstall = ''
      install message.html.tmpl -Dt $out
    '';
  };
in
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
      MX_MSG_TEMPLATE = "${matrix-hook}/message.html.tmpl";
    };
    serviceConfig = {
      EnvironmentFile = [
        # format: MX_TOKEN=<token>
        config.sops.secrets.nix-community-bot-access-token.path
      ];
      Type = "simple";
      ExecStart = "${matrix-hook}/bin/matrix-hook";
      Restart = "always";
      RestartSec = "10";
      DynamicUser = true;
      User = "matrix-hook";
      Group = "matrix-hook";
    };
  };
}
