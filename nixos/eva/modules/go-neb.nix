{ config, lib, pkgs, ... }:

{
  services.go-neb = {
    enable = true;
    baseUrl = "https://localhost";
    # ACCESS_TOKEN=<SECRET>
    config = {
      clients = [{
        UserID = "@nix-community-alerts:matrix.org";
        AccessToken = "$ACCESS_TOKEN";
        DeviceID = "eva";
        HomeserverURL = "https://matrix.org";
        Sync = true;
        AutoJoinRooms = true;
        DisplayName = "Alertmanager";
        AcceptVerificationFromUsers = [ ":localhost:8008" ];
      }];
      services = [{
        ID = "alertmanager_service";
        Type = "alertmanager";
        UserID = "@nix-community-alerts:matrix.org";
        Config = {
          # This is for information purposes only. It should point to Go-NEB path as follows:
          # `/services/hooks/<base64 encoded service ID>`
          # Where in this case "service ID" is "alertmanager_service"
          # Make sure your BASE_URL can be accessed by the Alertmanager instance!
          webhook_url = "https://localhost/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2U";
          # Each room will get the notification with the alert rendered with the given template
          rooms."!PbtOpdWBSRFbEZRLIf:numtide.com" = {
            text_template = "{{range .Alerts -}}
 [{{ .Status }}] {{index .Labels \"alertname\" }}: {{index .Annotations \"description\"}}
 {{ end -}}";
            msg_type = "m.text";
          };
        };
      }];
    };
    secretFile = config.sops.secrets.go-neb-secrets.path;
  };

  sops.secrets.go-neb-secrets = {};
}
