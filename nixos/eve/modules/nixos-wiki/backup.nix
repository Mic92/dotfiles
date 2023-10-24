{ config, pkgs, ... }:
let
  wikiDump = "/var/backup/wikidump.xml.gz";

  mediawiki-maintenance = pkgs.runCommand "mediawiki-maintenance"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
      preferLocalBuild = true;
    } ''
    mkdir -p $out/bin
    makeWrapper ${pkgs.php}/bin/php $out/bin/mediawiki-maintenance \
      --set MEDIAWIKI_CONFIG ${config.services.phpfpm.pools.mediawiki.phpEnv.MEDIAWIKI_CONFIG} \
      --add-flags ${config.services.mediawiki.finalPackage}/share/mediawiki/maintenance/run.php
  '';

  wiki-restore = pkgs.writeShellApplication {
    name = "wiki-restore";
    runtimeInputs = [
      pkgs.postgresql
      pkgs.coreutils
      pkgs.util-linux
      mediawiki-maintenance
    ];
    text = ''
      tmpdir=$(mktemp -d)
      cleanup() { rm -rf "$tmpdir"; }
      cd "$tmpdir"
      chown mediawiki:nginx "$tmpdir"

      rm -rf /var/lib/mediawiki-uploads
      install -d -m 755 -o mediawiki -g nginx /var/lib/mediawiki-uploads
      systemctl stop phpfpm-mediawiki.service
      runuser -u postgres -- dropdb mediawiki
      systemctl restart postgresql
      systemctl restart mediawiki-init.service
      cat <<EOF | runuser -u mediawiki -- mediawiki-maintenance deleteBatch.php
      Main_Page
      MediaWiki:About
      EOF
      trap cleanup EXIT
      cp ${wikiDump} "$tmpdir"
      chown mediawiki:nginx "$tmpdir/wikidump.xml.gz"
      chmod 644 "$tmpdir/wikidump.xml.gz"
      runuser -u mediawiki -- mediawiki-maintenance importDump.php --uploads "$tmpdir/wikidump.xml.gz"
      runuser -u mediawiki -- mediawiki-maintenance rebuildrecentchanges.php
      systemctl start phpfpm-mediawiki.service
    '';
  };
in
{
  environment.systemPackages = [ mediawiki-maintenance ];

  systemd.services.wiki-backup = {
    startAt = "hourly";

    serviceConfig = {
      ExecStart = [
        "${pkgs.wget}/bin/wget https://nixos.wiki/images/wikidump.xml.gz -O ${wikiDump}.new"
        "${pkgs.coreutils}/bin/mv ${wikiDump}.new ${wikiDump}"
      ];
      Type = "oneshot";
    };
  };

  systemd.services.wiki-restore = {
    startAt = "daily";
    path = [ pkgs.postgresql mediawiki-maintenance ];

    serviceConfig = {
      ExecStart = "${wiki-restore}/bin/wiki-restore";
      Type = "oneshot";
    };
  };

  services.nginx.virtualHosts.${config.services.mediawiki.nginx.hostName} = {
    locations."=/wikidump.xml.gz".alias = wikiDump;
  };
}
