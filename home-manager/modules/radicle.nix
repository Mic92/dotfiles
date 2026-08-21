{
  config,
  pkgs,
  lib,
  ...
}:

lib.mkMerge [
  {
    home.packages = [
      pkgs.radicle-node
    ]
    # Not packaged for darwin yet.
    ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
      pkgs.radicle-desktop
    ];
  }

  # Keep the node running so patches and follows stay in sync.
  (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    systemd.user.services.radicle-node = {
      Unit.Description = "Radicle node";
      Service = {
        ExecStart = "${pkgs.radicle-node}/bin/radicle-node --force";
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = [ "default.target" ];
    };
  })

  (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    launchd.enable = true;
    launchd.agents.radicle-node = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.radicle-node}/bin/radicle-node"
          "--force"
        ];
        KeepAlive = true;
        RunAtLoad = true;
        ProcessType = "Background";
        StandardOutPath = "${config.home.homeDirectory}/.local/state/radicle-node.log";
        StandardErrorPath = "${config.home.homeDirectory}/.local/state/radicle-node.err";
      };
    };
  })
]
