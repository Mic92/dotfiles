{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  cfg = config.services.hide-nix-store;
in
{
  options.services.hide-nix-store = {
    enable = lib.mkEnableOption "eBPF LSM that hides /nix/store from selected uids";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.hide-nix-store;
      defaultText = lib.literalExpression "self.packages.\${system}.hide-nix-store";
    };

    uids = lib.mkOption {
      type = lib.types.listOf lib.types.int;
      default = [ ];
      example = [ 1000 ];
      description = ''
        UIDs for which /nix/store is made inaccessible (open/read return
        EPERM, directory listings appear empty/denied). Additional uids
        can be added at runtime via the pinned map at
        /sys/fs/bpf/hide-nix-store/target_uids.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.security.lsm == null || lib.elem "bpf" config.security.lsm;
        message = "services.hide-nix-store requires the bpf LSM to be active (security.lsm or lsm= kernel param)";
      }
    ];

    # Make sure the BPF LSM is in the chain.
    security.lsm = lib.mkDefault [
      "landlock"
      "yama"
      "bpf"
    ];

    systemd.services.hide-nix-store = {
      description = "Hide /nix/store from selected uids via eBPF LSM";
      wantedBy = [ "multi-user.target" ];
      after = [ "sysinit.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${lib.getExe cfg.package} ${lib.concatMapStringsSep " " toString cfg.uids}";
        Restart = "on-failure";
        # Needs real root caps to load BPF_PROG_TYPE_LSM.
        AmbientCapabilities = [
          "CAP_BPF"
          "CAP_PERFMON"
          "CAP_SYS_ADMIN"
          "CAP_SYS_RESOURCE"
        ];
      };
    };

    environment.systemPackages = [ pkgs.bpftools ];
  };
}
