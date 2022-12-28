{ pkgs, ... }: {
  systemd.tmpfiles.rules = [
    "d /var/lib/backup/ 0700 - - - -"
  ];
  systemd.services.wiki-backup = {
    startAt = "hourly";

    serviceConfig = {
      ExecStart = "${pkgs.wget}/bin/wget https://nixos.wiki/images/wikidump.xml.gz -O /var/lib/backup/wikidump.xml.gz";
    };
  };
}
