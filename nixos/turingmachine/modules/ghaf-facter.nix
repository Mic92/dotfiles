{ config, lib, ... }:
let
  isPciDevice = dev: dev.bus_type.name == "PCI";
  pciAudioDevices = lib.filter isPciDevice config.facter.report.hardware.sound;
  networkInterfaces = lib.filter isPciDevice config.facter.report.hardware.network_controller;
  pciGPUs = lib.filter isPciDevice config.facter.report.hardware.graphics_card;

  # FIXME: network devices don't have sysfs_bus_id ?
  extractBusId = dev: builtins.elemAt (builtins.match ".*/([^/]+)$" dev.sysfs_device_link) 0;

  mapPciDevice = dev: {
    name = dev.name or null;
    path = if dev ? sysfs_bus_id then dev.sysfs_bus_id else extractBusId dev;
    vendorId = lib.toHexString dev.vendor.value;
    productId = lib.toHexString dev.device.value;
  };
in
{
  ghaf.hardware.definition = {
    # System name
    name = config.facter.report.smbios.system.product;
    skus = [
      "${config.facter.report.smbios.board.product} ${config.facter.report.smbios.system.product}"
    ];

    #input = {
    #    keyboard = {
    #        name = [];
    #        evdev = [];
    #    };

    #    mouse = {
    #        name = [];
    #        evdev = [];
    #    };

    #    touchpad = {
    #        name = [];
    #        evdev = [];
    #    };

    #    misc = {
    #        name = [];
    #        evdev = [];
    #    };
    #};

    audio = {
      pciDevices = builtins.map mapPciDevice pciAudioDevices;
      kernelConfig = {
        stage1.kernelModules = [ ];
        stage2.kernelModules = builtins.concatMap (dev: dev.driver_modules) pciAudioDevices;
        kernelParams = [ ];
      };
    };

    gpu = {
      pciDevices = builtins.map mapPciDevice pciGPUs;
      kernelConfig = {
        stage1.kernelModules = [ ];
        stage2.kernelModules = builtins.concatMap (dev: dev.driver_modules) pciGPUs;
        kernelParams = [ "earlykms" ];
      };
    };

    network = {
      pciDevices = builtins.map mapPciDevice pciAudioDevices;
      kernelConfig = {
        stage1.kernelModules = [ ];
        stage2.kernelModules = builtins.concatMap (dev: dev.driver_modules) networkInterfaces;
        kernelParams = [ ];
      };
    };

    usb = {
      internal = [ ];
      external = [
        # Add external USB devices here
      ];
    };
  };
}
