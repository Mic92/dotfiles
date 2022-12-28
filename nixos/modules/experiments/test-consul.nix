{ lib, ... }:
let
  sharedSettings = {
    ephemeral = true;
    autoStart = true;
    privateNetwork = true;
    hostAddress = "192.168.100.1";
  };
  sharedModule = {
    system.stateVersion = "22.11";
  };
  consulAgent = {
    services.consul.enable = true;
    services.consul.extraConfig = {
      retry_join = [
        "192.168.100.2"
        "192.168.100.3"
        "192.168.100.4"
      ];
      acl = {
        enabled = true;
        default_policy = "deny";
        enable_token_persistence = true;
      };
    };
  };
  consulServer = {
    imports = [ consulAgent ];
    networking.firewall = {
      allowedTCPPorts = [
        8301 # lan serf
        8302 # wan serf
        #8600 # dns
        8500 # http api
        8300 # RPC address
      ];
      allowedUDPPorts = [
        8301 # lan serf
        8302 # wan serf
        #8600 # dns
      ];
    };
    services.consul.extraConfig = {
      bootstrap_expect = 3;
      server = true;
    };
  };
in
{
  containers.consul1 =
    sharedSettings
    // {
      localAddress = "192.168.100.2";
      config = {
        imports = [
          sharedModule
          consulServer
        ];
      };
    };

  containers.consul2 =
    sharedSettings
    // {
      localAddress = "192.168.100.3";
      config = {
        imports = [
          sharedModule
          consulServer
        ];
      };
    };

  containers.consul3 =
    sharedSettings
    // {
      localAddress = "192.168.100.4";
      config = {
        imports = [
          sharedModule
          consulServer
        ];
      };
    };

  containers.vault0 =
    sharedSettings
    // {
      localAddress = "192.168.100.6";
      privateNetwork = false;
      config = {
        imports = [
          sharedModule
          consulAgent
        ];
        networking.firewall.allowedTCPPorts = [
          8300
        ];
        services.vault = {
          enable = true;
          dev = true;
          devRootTokenID = "phony-secret";
        };
        services.consul.interface.bind = "dummy1";
        systemd.services.consul.after = lib.mkForce [ ];
        systemd.services.consul.bindsTo = lib.mkForce [ ];
      };
    };
}
