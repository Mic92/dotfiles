{ self, pkgs, ... }:
{
  home.packages = [
    (pkgs.writeShellApplication {
      name = "kimai";
      runtimeInputs = [
        self.packages.${pkgs.stdenv.hostPlatform.system}.kimai-cli
        pkgs.rbw
        pkgs.jq
      ];
      text = ''
        # Build config JSON from rbw on the fly, so the API key never sits on disk
        config=$(mktemp --suffix=.json)
        trap 'rm -f "$config"' EXIT
        token=$(rbw get "Kimai API")
        jq -n --arg token "$token" '{URL: "https://kimai.thalheim.io/", API_TOKEN: $token}' > "$config"
        KIMAI_CONFIG="$config" exec kimai "$@"
      '';
    })
  ];
}
