{config, ...}: {
  services.nginx.virtualHosts."go-neb.thalheim.io" = {
    forceSSL = true;
    enableACME = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:4050;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Host $host:443;
      proxy_set_header X-Forwarded-Server $host;
      proxy_set_header X-Forwarded-Port 443;
      proxy_set_header X-Forwarded-Proto $scheme;
    '';
  };
  services.go-neb = {
    enable = true;
    baseUrl = "https://go-neb.thalheim.io";
    #baseUrl = "https://localhost";
    # ACCESS_TOKEN=<SECRET>
    config = {
      clients = [
        {
          UserID = "@nix-community-bot:nixos.dev";
          AccessToken = "$ACCESS_TOKEN";
          DeviceID = "eva";
          HomeserverURL = "https://matrix.nixos.dev";
          Sync = true;
          AutoJoinRooms = true;
          DisplayName = "Alertmanager";
          AcceptVerificationFromUsers = [":localhost:8008"];
        }
      ];
      realms = [
        {
          ID = "github_realm";
          Type = "github";
          Config = {};
        }
      ];
      sessions = [
        {
          SessionID = "your_github_session";
          RealmID = "github_realm";
          UserID = "@mic92:nixos.dev";
          Config = {
            # Populate these fields by generating a "Personal Access Token" on github.com
            AccessToken = "$GITHUB_TOKEN";
            Scopes = "admin:org_hook,admin:repo_hook,repo,user";
          };
        }
      ];
      services = [
        {
          ID = "github_webhook_service";
          Type = "github-webhook";
          UserID = "@nix-community-bot:nixos.dev";
          Config = {
            RealmID = "github_realm";
            ClientUserID = "@mic92:nixos.dev";
            Rooms."!cBybDCkeRlSWfuaFvn:numtide.com".Repos = {
              "nix-community/infra".Events = ["push" "issues" "pull_request"];
            };
          };
        }
        {
          ID = "alertmanager_service";
          Type = "alertmanager";
          UserID = "@nix-community-bot:nixos.dev";
          Config = {
            # This is for information purposes only. It should point to Go-NEB path as follows:
            # `/services/hooks/<base64 encoded service ID>`
            # Where in this case "service ID" is "alertmanager_service"
            # Make sure your BASE_URL can be accessed by the Alertmanager instance!
            webhook_url = "https://go-neb.thalheim.io/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2U";
            # Each room will get the notification with the alert rendered with the given template
            Rooms."!cBybDCkeRlSWfuaFvn:numtide.com" = {
              text_template = "{{range .Alerts -}}
 [{{ .Status }}] {{index .Labels \"alertname\" }}: {{index .Annotations \"description\"}}
 {{ end   -}}";
              msg_type = "m.text";
            };
          };
        }
      ];
    };
    secretFile = config.sops.secrets.go-neb-secrets.path;
  };

  sops.secrets.go-neb-secrets = {};
}
