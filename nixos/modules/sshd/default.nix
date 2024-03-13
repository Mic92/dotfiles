{ config
, pkgs
, ...
}:
{
  imports = [
    ../ssh.nix
  ];

  # srvos sets more sane defaults
  services.openssh = {
    enable = true;
    settings.HostCertificate = config.clanCore.secrets.openssh-interactive.facts."ssh.id_ed25519-cert.pub".path;
  };

  clanCore.secrets.openssh-interactive = {
    facts."ssh.id_ed25519-cert.pub" = { };
    secrets = { };
    generator.path = [ pkgs.coreutils pkgs.openssh ];
    generator.prompt = ''
      Provide a the host ssh certificate public key ${config.networking.hostName}
      Run this script:
      ${pkgs.writeShellScript "generate" ''
        temp=$(mktemp -d)
        trap "rm -rf $temp" EXIT
        oldpwd=$(pwd)
        cd $temp
        ca_dir=$HOME/git/doctor-cluster-config/modules/sshd

        hosts=${config.networking.hostName},${config.networking.hostName}.i,${config.networking.hostName}.r,${config.networking.hostName}.thalheim.io
        (
         umask 077
         ${pkgs.sops}/bin/sops --extract '["ssh-ca"]' -d "$ca_dir/ca-keys.yml" > ./ssh-ca
        )

        pubkey="$oldpwd/machines/${config.networking.hostName}/facts/ssh.id_ed25519.pub"
        cp "$pubkey" ./ssh.pub
        ssh-keygen -h -s ./ssh-ca -n "$hosts" -I $(basename "$pubkey" .pub) ./ssh.pub
        cat ssh-cert.pub | wl-copy
      ''}
    '';
    generator.script = ''
      echo "$prompt_value" > $facts/ssh.id_ed25519-cert.pub
    '';
  };
}
