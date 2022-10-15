{ pkgs, lib, ...}:

let
  nsncdPackage = pkgs.rustPlatform.buildRustPackage rec {
    pname = "nsncd";
    version = "unstable-2021-10-13";

    src = pkgs.fetchFromGitHub {
      owner = "flokli";
      repo = "nsncd";
      rev = "6168ba3ecba7f1d12f331fe28452cc16b99376f9";
      hash = "sha256-zLCG2BtOqQMFKVBblrhie/voagGm0QM7v+g3k3QABu8=";
    };

    cargoSha256 = "sha256-lBK5Q/+FAvxgTQEudbYOGKJGtw0Lx5UJgUP8GTjAqAs=";
    #buildType = "debug";
  };
in {
  systemd.services.nscd.serviceConfig.ExecStart = lib.mkForce "${nsncdPackage}/bin/nsncd";
  systemd.services.nscd.serviceConfig.Type = lib.mkForce "notify";
  systemd.services.nscd.serviceConfig.ExecReload = lib.mkForce [];
}
