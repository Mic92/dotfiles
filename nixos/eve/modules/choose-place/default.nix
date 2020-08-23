{ pkgs, ... }: {
  services.uwsgi = {
    enable = true;
    plugins = [ "python3" ];
    instance = {
      type = "emperor";
      vassals.choose-place = {
        type = "normal";
        strict = true;
        uid = "choose-place";
        gid = "choose-place";
        enable-threads = true;
        module = "choose_place:create_app()";
        socket = "/run/uwsgi/choose-place.sock";
        chmod-socket = 664;
        pythonPackages = self: [
          (self.toPythonModule pkgs.choose-place)
        ];
      };
    };
  };

  users.users.choose-place = {
    isSystemUser = true;
    group = "choose-place";
  };
  users.groups."choose-place" = {};
  users.users.nginx.extraGroups = [ "uwsgi" ];
  systemd.services.nginx.serviceConfig.SupplementaryGroups = [ "uwsgi" ];

  services.nginx = {
    virtualHosts."choose-place.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        uwsgi_pass unix:/run/uwsgi/choose-place.sock;
      '';
    };
  };
}
