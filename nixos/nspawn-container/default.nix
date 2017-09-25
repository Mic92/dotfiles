
{ config, pkgs, lib, ... }:

with lib;
let
  createContainer = {
    name,
    modules ? [],
    extraFiles ? "",
  }:
  let
     root = "/var/lib/machines/${name}";
     defaultConfig = {
       boot.isContainer = true;
       networking.hostName = mkDefault name;
       networking.useDHCP = false;
       systemd.network.enable = true;

       environment.systemPackages = [ ];
     };

     config = (import <nixpkgs/nixos/lib/eval-config.nix> {
       modules = [ defaultConfig ] ++ modules;
       prefix = [ "containers" name ];
     }).config;

     # /etc/.os-release is dummy and will be replaced init stage-2
     # with a symlink to /etc/static/os-release
     containerFiles = pkgs.writeScript "container-files" ''
       d "${root}" 0755 - - -
       d "${root}/etc" 0755 - - -
       L+ "${root}/etc/os-release" - - - - /etc/.os-release
       f "${root}/etc/.os-release" - - - -
       d "${root}/var/lib/private" 0700 - - - -
       w "${root}/etc/resolv.conf" 0700 - - - nameserver 8.8.8.8
       d "${root}/nix/var/nix/" 0755 - - -
       d "${root}/root" 0700 - - -
       d "${root}/sbin" 0755 - - -
       L+ "${root}/sbin/init" - - - - ${config.system.build.toplevel}/init
       L "/nix/var/nix/profiles/per-container/${name}" - - - - ${root}/nix/var/nix/profiles/
       L "/nix/var/nix/gcroots/per-container/${name}" - - - - ${root}/nix/var/nix/gcroots/
       ${extraFiles}
     '';
  in {
    systemd.nspawn."${name}" = {
      execConfig = {
        PrivateUsers=false;
        #NotifyReady=true;
      };
      filesConfig = {
        Bind = [
          "/nix/var/nix/profiles/per-container/${name}:/nix/var/nix/profiles"
          "/nix/var/nix/gcroots/per-container/${name}:/nix/var/nix/gcroots"
        ];
        BindReadOnly = [
          "/nix/store"
          "/nix/var/nix/db"
          "/nix/var/nix/daemon-socket"
        ];
      };
    };

    # copy systemd-nspawn@ to systemd-nspawn-${name} to allow overrides
    systemd.packages = [
      (pkgs.runCommand "systemd-nspawn-${name}" {} ''
        mkdir -p $out/lib/systemd/system/
        sed -e 's/%i/${name}/g' ${config.systemd.package}/example/systemd/system/systemd-nspawn@.service \
          > $out/lib/systemd/system/systemd-nspawn-${name}.service
      '')
    ];

    systemd.services."systemd-nspawn-${name}".serviceConfig = {
      ExecStartPre = "${config.systemd.package}/bin/systemd-tmpfiles --create ${containerFiles}";
      ExecStart = [""
      ''
        ${config.systemd.package}/bin/systemd-nspawn \
          --keep-unit \
          --machine=${name} \
          --directory="${root}" \
          --settings=override \
          --network-veth \
          ${config.system.build.toplevel}/init
      ''];
    };
  };
in createContainer {
  name = "foo";
}
