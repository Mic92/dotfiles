let
  credentials = {
    project = "evaa-163222";
    serviceAccount = "718643087322-compute@developer.gserviceaccount.com";
    accessKey = "/etc/nixos/secrets/eva.pem";
    region = "us-east1-d";
  };

in {
  network.description = "Monitoring server";

  resources = {
    gceStaticIPs.eva-ip = credentials // {
      region = "us-east1";
    };
    gceNetworks.eva-network = credentials // {
      addressRange = "10.142.0.0/24";
      firewall.allow-everything = {
        allowed = { tcp = null; udp = null; };
      };
    };
  };

  eva = { resources, lib, pkgs, ... }: {
    deployment.targetEnv = "gce";
    deployment.gce = credentials // {
      instanceType = "f1-micro";
      rootDiskSize = 29;
      ipAddress = resources.gceStaticIPs.eva-ip;
      network = resources.gceNetworks.eva-network;
    };

    services.caddy = {
      email = "joerg@thalheim.io";
      enable = true;
      config = ''
        monit.thalheim.io
        root /var/www
      '';
    };
    systemd.services.caddy = {
      serviceConfig.PermissionsStartOnly = true;
      preStart = ''
        mkdir -p /var/www
        cat > /var/www/index.html <<EOF 
        <html>
        <head><title>Hello world!</title></head>
        <body><h1>Hello world!</h1></body>
        </html>
        EOF
      '';
    };

    programs = {
      zsh = {
        enable = true;
        syntax-highlighting.enable = true;
        enableAutosuggestions = true;
        enableCompletion = true;
        oh-my-zsh = {
          enable = true;
          theme = "robbyrussell";
        };
      };
    };

    environment.systemPackages = with pkgs; [ 
      htop
      vim
    ];

    users.extraUsers = let
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine";
    in {
      joerg = {
        isNormalUser = true;
        home = "/home/joerg";
        extraGroups = [ "wheel" ];
        shell = "/run/current-system/sw/bin/zsh";
        uid = 1000;
        openssh.authorizedKeys.keys = [ key ];
      };
      root.openssh.authorizedKeys.keys = [ key ];
    };

    # size
    programs.ssh.setXAuthLocation = false;
    security.pam.services.su.forwardXAuth = lib.mkForce false;
    fonts.fontconfig.enable = false;
    services.nixosManual.enable = false;

    networking.dhcpcd.enable = false;
    systemd.network = {
      enable = true;
      networks.ethernet.extraConfig = ''
        [Match]
        Name = eth0

        [Network]
        DHCP = ipv4

        [DHCP]
        UseDomains = true
        UseMTU = true
      '';
    };
  };
}
