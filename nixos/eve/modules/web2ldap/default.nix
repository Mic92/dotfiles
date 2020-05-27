{ pkgs, config, ... }: {
  services.uwsgi = {
    enable = true;
    plugins = [ "python3" ];
    instance = {
      type = "emperor";
      vassals.web2ldap = {
        type = "normal";
        strict = true;
        uid = "web2ldap";
        gid = "web2ldap";
        enable-threads = true;
        # SCRIPT_NAME is not set correctly otherwise
        mount = "/ldap=web2ldap.wsgi:application";
        route = "^/$ redirect:/ldap";
        manage-script-name = true;
        socket = "/run/uwsgi/web2ldap.sock";
        chmod-socket = 664;
        pythonPackages = let
          web2ldap = pkgs.nur.repos.mic92.python3Packages.web2ldap;
        in self: [
          web2ldap
          (self.buildPythonPackage {
            name = "web2ldapcnf";
            src = ./.;
            doCheck = false;
            postPatch = ''
              ln -s ${web2ldap}/etc/web2ldap/web2ldapcnf/* web2ldapcnf/
            '';
          })
        ];
      };
    };
  };

  users.users.web2ldap = {
    isSystemUser = true;
    group = "web2ldap";
  };
  users.groups."web2ldap" = {};
  users.users.nginx.extraGroups = [ "uwsgi" ];
  systemd.services.nginx.serviceConfig.SupplementaryGroups = [ "uwsgi" ];

  services.nginx = {
    virtualHosts."ldap.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/".extraConfig = ''
        include ${config.services.nginx.package}/conf/uwsgi_params;
        uwsgi_pass unix:/run/uwsgi/web2ldap.sock;
      '';
    };
    #commonHttpConfig = ''
    #  ldap_server web2ldap {
    #    url ldap://127.0.0.1:389/DC=test,DC=local?sAMAccountName?sub?(objectClass=person);
    #    binddn "";
    #    binddn_passwd ;
    #    group_attribute uniquemember;
    #    group_attribute_is_dn on;
    #    require user;
    #  }
    #'';
  };

  services.netdata.httpcheck.checks.web2ldap = {
    url = "https://ldap.thalheim.io";
    regex = "web2ldap";
  };

  services.icinga2.extraConfig = ''
    apply Service "Web2ldap v4 (eve)" {
      import "eve-http4-service"
      vars.http_vhost = "ldap.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }

    apply Service "Web2ldap v6 (eve)" {
      import "eve-http6-service"
      vars.http_vhost = "ldap.thalheim.io"
      vars.http_uri = "/"
      assign where host.name == "eve.thalheim.io"
    }
  '';
}
