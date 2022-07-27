{ config, lib, pkgs, ... }:

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
      bootstrap_expect = 3;
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
      server = true;
    };
  };
in
{

  containers.consul0 = sharedSettings // {
    localAddress = "192.168.100.2";
    config = { config, pkgs, ... }: {
      imports = [
        sharedModule
        consulServer
      ];
    };
  };

  containers.consul2 = sharedSettings // {
    localAddress = "192.168.100.3";
    config = { config, pkgs, ... }: {
      imports = [
        sharedModule
        consulServer
      ];
    };
  };

  containers.consul3 = sharedSettings // {
    localAddress = "192.168.100.4";
    config = { config, pkgs, ... }: {
      imports = [
        sharedModule
        consulServer
      ];
    };
  };

  containers.agent = sharedSettings // {
    localAddress = "192.168.100.5";
    config = { config, pkgs, ... }: {
      imports = [
        sharedModule
        consulAgent
      ];
    };
  };

  containers.vault0 = sharedSettings // {
    localAddress = "192.168.100.6";
    config = { config, pkgs, ... }: {
      imports = [
        sharedModule
      ];
      services.vault = {
        enable = true;
        dev = true;
        devRootTokenID = "phony-secret";
      };
    };
  };
}
