{
  config,
  pkgs,
  self,
  ...
}:
{
  imports = [ self.inputs.punchcard.nixosModules.punchcard ];

  # OIDC client for Authelia: punchcard gets the plaintext secret via env
  # file, Authelia gets the pbkdf2 digest (see authelia.nix).
  clan.core.vars.generators.punchcard-oidc = {
    files.env.owner = "punchcard";
    files.client-secret-hash.secret = false;
    runtimeInputs = with pkgs; [
      coreutils
      openssl
      authelia
      gnused
    ];
    script = ''
      secret=$(openssl rand -hex 32)
      echo "OIDC_CLIENT_SECRET=$secret" > "$out/env"
      authelia crypto hash generate pbkdf2 --variant sha512 \
        --password "$secret" |
        sed 's/^Digest: //' > "$out/client-secret-hash"
    '';
  };

  services.punchcard = {
    enable = true;
    package = self.inputs.punchcard.packages.${pkgs.stdenv.hostPlatform.system}.punchcard;
    port = 3009;
    database.type = "postgresql";
    oidc = {
      issuerUrl = "https://auth.thalheim.io";
      clientId = "punchcard";
      redirectUrl = "https://punchcard.thalheim.io/callback";
    };
    environmentFile = config.clan.core.vars.generators.punchcard-oidc.files.env.path;
  };

  services.nginx.virtualHosts."punchcard.thalheim.io" = {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/".extraConfig = ''
      proxy_pass http://localhost:3009;
      proxy_set_header Host $host;
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
    '';
  };
}
