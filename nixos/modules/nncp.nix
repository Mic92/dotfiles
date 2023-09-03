{ config, lib, pkgs, ... }:
let
  nodes = {
    eve = {
      id = "32BJZQY3HYDKL4F2FT3WRQGQRSNOEOLJRSLXBPEX7MDTCSWOJCJQ";
      exchpub = "4F2ZK4KXGBANDBL4XEO4CFDNNBKYPJCWKVTXFG6KIKBSVTTYKIPQ";
      signpub = "TZXAPW4IPVMKU2K23ZYCW6BDUZWMLCP6UDSQCZXLP3DHEEMNOD3Q";
      noisepub = "5UYKFAKYU7RG5CLN57WJUVXG3T7PC5KXACOHVDNUIIS33BFAUBTQ";
      addrs.internet = "eve.i:5400";
    };
    eva = {
      id = "YHZBK6LYPM6L6YBWZJRLSRMIEHI4ZM5WYYIKFDO2RR3BESULXT4Q";
      exchpub = "QDSEQD7ZIIU7L2YR37ZKOATG7GT6BC6VK2XKI2FS4HJJORUU4UVQ";
      signpub = "7HNVCSILQB2QIHPAWM2WQQFXA6STFGJZZT7DX6JZYQ3B3TGT63AQ";
      noisepub = "IBH6IIRZHC6E6WDKVG26CR3XY3YDRYBRVDKXYY6E3JHJWMDANB4Q";
      addrs.internet = "eva.i:5400";
    };
    turingmachine = {
      id = "5QX2ZVPHKAGCSETCTX5V5JFY6P7ZAPJ55CC3CVINQYD7WJBHVHYQ";
      exchpub = "LRHUBHPRIWEFUUVXXWANSPDVGCYCHY7F4YQ4XGFTHQUVYK7ZJEMQ";
      signpub = "JRWYU3LMGOUBPA4GFJQT4CJNFS67UENBN7BMMVVDH6RERGSXUT5Q";
      noisepub = "6IKRB2ICMURMGJLVVHCUZY5UCGG7ERRO55MYDAX4X5VXH7LDPB2Q";
    };
  };
  nodesJson = builtins.toFile "nodes.json" (builtins.toJSON nodes);
in
{
  networking.firewall.allowedTCPPorts = [ 5400 ];
  services.nncp.daemon = {
    enable = true;
    extraArgs = [ "-autotoss" "-autotoss-gen-ack" ];
    socketActivation.enable = true;
  };
  environment.systemPackages = [
    (pkgs.writeScriptBin "nncp-nix-update" ''
      #!${pkgs.bash}/bin/bash
      set -euo pipefail -x
      export PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.nncp config.nix.package pkgs.jq pkgs.gnutar pkgs.zstd pkgs.git ]}
      if [[ $# -lt 1 ]]; then
        echo "Usage: $0 <node>"
        exit 1
      fi
      if [[ $(id -u) -ne 0 ]]; then
        echo "Must be run as root"
        exit 1
      fi
      path=$(nix flake metadata --json | jq -r '.path')
      node=$1
      shift
      tar -C "$path" --zstd -cf - . | nncp-exec "$node" nixos-rebuild "$@"
      nncp-call "$node"
    '')
  ];
  programs.nncp = {
    enable = true;
    secrets = [ config.sops.secrets."${config.clanCore.machineName}-nncp".path ];
    settings.neigh = lib.mapAttrs
      (name: node: node // {
        exec = {
          "nixos-rebuild" = [
            (pkgs.writeShellScript "nixos-rebuild" ''
              export PATH=${lib.makeBinPath [ pkgs.nncp pkgs.gnutar pkgs.zstd ]}
              set -euo pipefail -x
              (if [[ "$NNCP_SENDER" == "${nodes.turingmachine.id}" ]]; then
                if ! tar -C /etc/nixos --zstd -x -f -; then
                  echo "failed to extract tarball"
                fi
                if ! /run/current-system/sw/bin/nixos-rebuild switch 2>&1; then
                  echo "failed to switch, see above"
                fi
              else
                echo "sender $NNCP_SENDER does not match turingmachine. Do not run update!"
              fi) | nncp-exec "$NNCP_SENDER" "nixos-rebuild-notify"
            '')
          ];
          "nixos-rebuild-notify" = [
            (pkgs.writeScript "nixos-rebuild-notify.py" ''
              #!${pkgs.python3}/bin/python
              import json
              import os
              import subprocess
              from pathlib import Path

              def main() -> None:
                  sender = os.environ["NNCP_SENDER"]

                  try:
                      message = input()
                  except EOFError:
                      message = "no message"

                  sender = f"unknown machine ({sender})"
                  nodes = json.loads(Path("${nodesJson}").read_text())
                  for name, data in nodes.items():
                      if data["id"] == sender:
                          sender = name
                          break
                      else:
                          print(data["id"], sender)
                  env = os.environ.copy()
                  env["XDG_RUNTIME_DIR"] = f"/run/user/${builtins.toString config.users.users.joerg.uid}"
                  env["DISPLAY"] = ":0"
                  env["PATH"] = "${lib.makeBinPath [ pkgs.libnotify pkgs.dbus pkgs.utillinux ]}"
                  subprocess.run(["runuser", "-u", "joerg", "notify-send", f"nixos-rebuild from {sender}"], env=env)
                  with open("/tmp/nixos-rebuild.log", "w") as f:
                    f.write(f"nixos-rebuild from {sender}:\n")
                    f.write(message)
                    if message[-1] != "\n":
                      f.write("\n")
              if __name__ == "__main__":
                  main()
            '')
          ];
        };
        addrs.tinc = "${name}.r:5400";
      })
      (nodes // {
        self = nodes.${config.networking.hostName};
      });
  };
}
