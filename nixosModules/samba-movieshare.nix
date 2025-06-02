{
  pkgs,
  config,
  ...
}:
{
  services.samba = {
    enable = true;
    package = pkgs.samba;
    openFirewall = true;
    settings = {
      global = {
        security = "user";
        workgroup = "WORKGROUP";
        "server string" = "Shannan's Movies";
        "max log size" = "50";
        "dns proxy" = false;
        "syslog only" = true;
      };
      public = {
        comment = "Shannan's Movies";
        path = "/home/shannan/yellow.r";
        "force user" = "shannan";
        "force group" = "users";
        public = "yes";
        "guest ok" = "no";
        #"only guest" = "yes";
        "create mask" = "0644";
        "directory mask" = "2777";
        writable = "yes";
        browseable = "yes";
        printable = "no";
        "valid users" = "shannan";
      };
    };
  };

  users.users.shannan.isNormalUser = true;

  clan.core.vars.generators.shannan-samba = {
    files.password = { };
    runtimeInputs = with pkgs; [
      coreutils
      xkcdpass
      mkpasswd
    ];
    script = ''
      xkcdpass --numwords 3 --delimiter - --count 1 > $out/password
    '';
  };

  systemd.services.samba-smbd.postStart =
    let
      shannan-samba = config.clan.core.vars.generators.shannan-samba.files.password.path;
    in
    ''
      (echo $(< ${shannan-samba}); echo $(< ${shannan-samba})) | ${config.services.samba.package}/bin/smbpasswd -s -a shannan
    '';

  services.samba-wsdd = {
    enable = true;
    openFirewall = true;
  };

  services.avahi = {
    publish.enable = true;
    publish.userServices = true;
    # ^^ Needed to allow samba to automatically register mDNS records (without the need for an `extraServiceFile`)
    nssmdns4 = true;
    # ^^ Not one hundred percent sure if this is needed- if it aint broke, don't fix it
    enable = true;
    openFirewall = true;
  };

  systemd.tmpfiles.rules = [ "d /var/spool/samba 1777 root root -" ];
}
