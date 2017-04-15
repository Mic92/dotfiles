# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let

  userservice = args: {
    name = args.name;
    value = {
      wantedBy = [ "default.target" ];
      enable = true;
      serviceConfig = {
        RestartSec="500ms";
        ExecStart="/run/current-system/sw/bin/sh -c '" +
          "export GTK_DATA_PREFIX=/run/current-system/sw;" +
          "export PATH=$PATH:/var/setuid-wrappers:%h/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:/run/current-system/sw/sbin;" +
          "exec ${args.command}'";
      };
    };
  };

in {
  imports = [ # Include the results of the hardware scan.or
    ./hardware-configuration.nix
    ./network-configuration.nix
    ./bird.nix
    ./packages.nix
  ];


  boot = {
    plymouth.enable = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  environment.sessionVariables = {
    # so gtk2.0/gtk3.0 themes can be found
    GTK_DATA_PREFIX = "/run/current-system/sw";
    LD_LIBRARY_PATH = [ config.system.nssModules.path ];
  };

  nix = {
    gc.automatic = true;
    gc.dates = "03:15";
    nixPath = [ "/etc/nixos" "nixos-config=/etc/nixos/configuration.nix"];
    package = pkgs.nixUnstable;
    extraOptions = ''
      gc-keep-outputs = true
      gc-keep-derivations = true
      build-max-jobs = 10
      #binary-caches-parallel-connections = 3
      #connect-timeout = 10
      build-use-sandbox = true
    '';
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "en_DK.UTF-8";
  };
  time.timeZone = "Europe/Berlin";

  environment.etc."systemd/sleep.conf".text = pkgs.lib.mkAfter "SuspendState=freeze";

  boot.extraModprobeConfig = ''
    options zfs zfs_arc_max=34359738368
  '';

  services = {
    locate.enable = true;
    tor = {
      enable = true;
      extraConfig = ''
        DNSPort 9053
        AutomapHostsOnResolve 1
        AutomapHostsSuffixes .exit,.onion
        EnforceDistinctSubnets 1
        ExitNodes {de}
        NewCircuitPeriod 120
      '';
    };
    nscd.enable = true;
    zfs = {
      autoSnapshot.enable = true;
      autoScrub.enable = true;
    };

    openssh.enable = true;

    xserver = {
      enable = true;
      layout = "de";
      xkbOptions = "eurosign:e";
      libinput.enable = true;
      windowManager = {
        awesome = {
          enable = true;
          luaModules = with pkgs.lua52Packages; [ luasocket ];
        };
        default = "awesome";
      };
      videoDrivers = [ "modesetting" ];
      displayManager.sddm = {
        enable = true;
#        defaultUser = "joerg";
      };
    };

    avahi.enable = true;
    samba.enable = true;

    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.gutenprint pkgs.hplip ];
    };

    logind.extraConfig = ''
      LidSwitchIgnoreInhibited=no
      HandlePowerKey=ignore
    '';
    journald.extraConfig = "SystemMaxUse=1G";
  };

  #systemd.timers = {
  #  "update-wireguard-endpoint" = {
  #    wantedBy = ["multi-user.target"];
  #    timerConfig = {
  #      OnBootSec="1min";
  #      OnUnitActiveSec="1min";
  #    };
  #  };
  #};
  
  #systemd.package = pkgs.systemd.overrideDerivation(old: {
  #  src = builtins.fetchgit {
  #    url = /home/joerg/git/systemd;
  #    rev = "c87463cc115b1e4cf9771c0b0b5a9fa8b7ba1d8f";
  #  };
  #});

  powerManagment.powertop.enable = true;

  systemd.services = {
    systemd-networkd-wait-online.enable = false;
    caddy = let
      cfg = pkgs.writeText "Caddyfile" ''
        0.0.0.0 {
	  timeouts 0
          tls off
          markdown
          browse
          root /home/joerg/web

          basicauth /privat root cakeistasty
          basicauth /private root kuchenistlecker
        }
      '';
    in {
      description = "Caddy web server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''${pkgs.caddy.bin}/bin/caddy -conf=${cfg} -agree'';
        User = "joerg";
        AmbientCapabilities = "cap_net_bind_service";
      };
    };

    audio-off = {
      description = "Mute audio before suspend";
      wantedBy = [ "sleep.target" ];
      serviceConfig = {
        Type = "oneshot";
        Environment = "XDG_RUNTIME_DIR=/run/user/1000";
        User = "joerg";
        RemainAfterExit = "yes";
        ExecStart = "${pkgs.pamixer}/bin/pamixer --mute";
      };
    };

    wifi-scan = {
      description = "Scan wlan after suspend";
      wantedBy = [ "sleep.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = "yes";
        ExecStart = "${pkgs.coreutils}/bin/true";
        ExecStop = "${pkgs.wpa_supplicant}/bin/wpa_cli scan";
      };
    };
    btsync = with pkgs; {
      wantedBy = [ "multi-user.target" ];
      description = "Bittorrent Sync user service";
      after       = [ "network.target" "local-fs.target" ];
      serviceConfig = {
        Restart   = "on-abort";
        User = "joerg";
        ExecStart = "${pkgs.bittorrentSync20}/bin/btsync --nodaemon --config /home/joerg/.config/btsync/btsync.conf";
      };
    };
    godoc = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Environment = "GOPATH=/home/joerg/go";
        User = "joerg";
        RemainAfterExit = "yes";
        ExecStart = "/home/joerg/go/bin/godoc -http=:8081";
      };
    };
  };

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
      storageDriver = "zfs";
      extraOptions = "--iptables=false";
    };
  };

  #systemd.user.services = (builtins.listToAttrs (map userservice [
  #  { name = "chromium"; command = "${pkgs.chromium}/bin/chromium"; }
  #  { name = "thunderbird"; command = "${pkgs.thunderbird}/bin/thunderbird"; }
  #  #{ name = "gajim"; command = "${pkgs.gajim}/bin/gajim"; }
  #  { name = "gpg-agent"; command = "${pkgs.gnupg1compat}/bin/gpg-agent"; }
  #  { name = "copyq"; command = "${pkgs.copyq}/bin/copyq"; }
  #  { name = "xautolock"; command = "${pkgs.xautolock}/bin/xautolock -time 20 -locker %h/bin/i3lock.sh"; }
  #  { name = "mpd"; command = "${pkgs.mpd}/bin/mpd"; }
  #])) // {
  #  gajim = {
  #    wantedBy = [ "default.target" ];
  #    enable = true;
  #    path = with pkgs; [dnsutils mercurial];
  #    serviceConfig = {
  #      RestartSec="500ms";
  #      Environment="GTK_DATA_PREFIX=/run/current-system/sw";
  #      ExecStart="${pkgs.gajim}/bin/gajim";
  #    };
  #  };
  #};

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      league-of-moveable-type
      hack-font
      #emojione
      dejavu_fonts
      inconsolata
      ubuntu_font_family
      unifont
    ];
  };
  programs = {
    ssh.startAgent = true;
    light.enable = true;
    adb.enable = true;
    zsh = {
      enable = true;
      enableSyntaxHighlighting = true;
      enableAutosuggestions = true;
      enableCompletion = true;
    };
  };
  hardware.pulseaudio.enable = true;

  users.extraUsers.joerg = {
    isNormalUser = true;
    home = "/home/joerg";
    extraGroups = ["wheel" "docker" "plugdev" "vboxusers" "adbusers" "input"];
    shell = "/run/current-system/sw/bin/zsh";
    uid = 1000;
  };
  users.extraGroups.adbusers = {};

  security = {
    audit.enable = false;
    apparmor.enable = true;
    sudo.wheelNeedsPassword = false;
  };

  services.dbus.packages = [ pkgs.gnome3.dconf pkgs.gnome3.gnome_keyring ];

  system.stateVersion = "17.03";
}
