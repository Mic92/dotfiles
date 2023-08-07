{ config, pkgs, ... }:
let
  matrix-alertmanager-receiver = pkgs.buildGoModule rec {
    pname = "matrix-alertmanager-receiver";
    version = "0.1.2";
    src = pkgs.fetchFromSourcehut {
      owner = "~fnux";
      repo = "matrix-alertmanager-receiver";
      rev = version;
      hash = "sha256-F6Cn0lmASAjWGEBCmyLdfz4r06fDTEfZQcynfA/RRtI=";
    };
    vendorHash = "sha256-7tRCX9FzOsLXCTWWjLp3hr1kegt1dxsbCKfC7tICreo=";
  };
in
{
  sops.secrets.nix-community-bot-access-token = { };
  sops.templates."config.toml".content = ''
    Homeserver = "https://matrix.thalheim.io"
    TargetRoomID = "!cBybDCkeRlSWfuaFvn:numtide.com"
    MXID = "@nix-community-bot:thalheim.io"
    MXToken = "${config.sops.placeholder.nix-community-bot-access-token}"
    HTTPPort = 9088
    HTTPAddress = "127.0.0.1"
  '';
  sops.templates."config.toml".owner = "matrix-alertmanager-receiver";
  sops.templates."config.toml".group = "matrix-alertmanager-receiver";
  systemd.services.matrix-alertmanager-receiver = {
    description = "Matrix Alertmanager Receiver";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${matrix-alertmanager-receiver}/bin/matrix-alertmanager-receiver -config ${config.sops.templates."config.toml".path}";
      Restart = "always";
      RestartSec = "10";
      User = "matrix-alertmanager-receiver";
      Group = "matrix-alertmanager-receiver";
    };
  };
  users.users.matrix-alertmanager-receiver = {
    isSystemUser = true;
    group = "matrix-alertmanager-receiver";
  };
  users.groups.matrix-alertmanager-receiver = { };
}
