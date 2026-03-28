{
  lib,
  self,
  pkgs,
  ...
}:
let
  micsSkills = self.inputs.mics-skills;
  micsSkillsPkgs = micsSkills.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  imports = [
    self.inputs.opencrow.nixosModules.default
    ./rbw.nix
    ./nostr.nix
    ./gitea.nix
    ./kagi.nix
    ./gmaps.nix
    ./mail.nix
    ./calendar.nix
    ./n8n.nix
    ./paperless.nix
    ./forge.nix
  ];

  config = {
    containers.opencrow.config.users.users.opencrow.uid = 2000;
    containers.opencrow.config.users.groups.opencrow.gid = 2000;
    users.groups.opencrow.gid = 2000;

    containers.opencrow.config.environment.etc."timezone".text = "Europe/Berlin\n";

    containers.opencrow.config.systemd.tmpfiles.rules = [
      "d /var/lib/opencrow/.config 0750 opencrow opencrow -"
    ];

    services.opencrow.skills =
      lib.genAttrs [
        "context7-cli"
        "db-cli"
        "n8n-cli"
        "pexpect-cli"
        "weather-cli"
      ] (name: "${micsSkills}/skills/${name}")
      // {
        http = ./skills/http;
      };

    services.opencrow = {
      enable = true;
      piPackage = self.inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi;

      extensions = {
        memory = true;
      };
      environment = {
        TZ = "Europe/Berlin";
        OPENCROW_SOUL_FILE = "${./soul.md}";
        OPENCROW_LOG_LEVEL = "debug";
        OPENCROW_PI_PROVIDER = "anthropic";
        OPENCROW_PI_MODEL = "claude-sonnet-4-6";
      };
      extraPackages = [
        micsSkillsPkgs.context7-cli
        micsSkillsPkgs.db-cli
        micsSkillsPkgs.n8n-cli
        micsSkillsPkgs.pexpect-cli
        micsSkillsPkgs.weather-cli
      ]
      ++ (with pkgs; [
        bc
        cacert
        coreutils
        curl
        diffutils
        fd
        file
        findutils
        gnugrep
        gnused
        gnutar
        gzip
        htmlq
        hurl
        git
        jq
        less
        libarchive
        openssh
        patch
        procps
        python3
        ripgrep
        tree
        unzip
        util-linux
        w3m
        wget
        which
        xz
        yq-go
        zip
        zstd
      ]);
    };
  };
}
