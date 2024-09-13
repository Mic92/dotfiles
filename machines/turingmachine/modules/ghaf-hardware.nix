# Copyright 2022-2024 TII (SSRC) and the Ghaf contributors
# SPDX-License-Identifier: Apache-2.0
#
# Module for Hardware Definitions
#
# The point of this module is to only store information about the hardware
# configuration, and the logic that uses this information should be elsewhere.
{ lib, ... }:
let
  inherit (lib) mkOption types literalExpression;
in
{
  options.ghaf.hardware.definition =
    let
      pciDevSubmodule = types.submodule {
        options = {
          path = mkOption {
            type = types.str;
            description = ''
              PCI device path
            '';
          };
          vendorId = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              PCI Vendor ID (optional)
            '';
          };
          productId = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              PCI Product ID (optional)
            '';
          };
          name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              PCI device name (optional)
            '';
          };
        };
      };

      # USB device submodule, defined either by product ID and vendor ID, or by bus and port number
      usbDevSubmodule = types.submodule {
        options = {
          name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              USB device name. NOT optional for external devices, in which case it must not contain spaces
              or extravagant characters.
            '';
          };
          vendorId = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              USB Vendor ID (optional). If this is set, the productId must also be set.
            '';
          };
          productId = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              USB Product ID (optional). If this is set, the vendorId must also be set.
            '';
          };
          hostbus = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              USB device bus number (optional). If this is set, the hostport must also be set.
            '';
          };
          hostport = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              USB device device number (optional). If this is set, the hostbus must also be set.
            '';
          };
        };
      };

      # Input devices submodule
      inputDevSubmodule = types.submodule {
        options = {
          name = mkOption {
            type = types.listOf types.any;
            default = [ ];
            description = ''
              List of input device names. Can either be a string, or a list of strings.
              The list option allows to bind several input device names to the same evdev.
              This allows to create one generic hardware definition for multiple SKUs.
            '';
          };
          evdev = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = ''
              List of event devices.
            '';
          };
        };
      };

      # Kernel configuration submodule
      kernelConfig = types.submodule {
        options = {
          stage1 = {
            kernelModules = mkOption {
              description = "Hardware specific kernel modules";
              type = types.listOf types.str;
              default = [ ];
              example = literalExpression ''
                [
                  "i915"
                ]
              '';
            };
          };
          stage2 = {
            kernelModules = mkOption {
              description = "Hardware specific kernel modules";
              type = types.listOf types.str;
              default = [ ];
              example = literalExpression ''
                [
                  "i915"
                ]
              '';
            };
          };
          kernelParams = mkOption {
            description = "Hardware specific kernel parameters";
            type = types.listOf types.str;
            default = [ ];
            example = literalExpression ''
              [
                "intel_iommu=on,sm_on"
                "iommu=pt"
                "module_blacklist=i915"
                "acpi_backlight=vendor"
                "acpi_osi=linux"
              ]
            '';
          };
        };
      };
    in
    {
      name = mkOption {
        description = "Name of the hardware";
        type = types.str;
        default = "";
      };

      skus = mkOption {
        description = "List of hardware SKUs (Stock Keeping Unit) covered with this definition";
        type = types.listOf types.str;
        default = [ ];
      };

      host = {
        kernelConfig = mkOption {
          description = "Host kernel configuration";
          type = kernelConfig;
          default = { };
        };
      };

      input = {
        keyboard = mkOption {
          description = "Name of the keyboard device(s)";
          type = inputDevSubmodule;
          default = { };
        };

        mouse = mkOption {
          description = "Name of the mouse device(s)";
          type = inputDevSubmodule;
          default = { };
        };

        touchpad = mkOption {
          description = "Name of the touchpad device(s)";
          type = inputDevSubmodule;
          default = { };
        };

        misc = mkOption {
          description = "Name of the misc device(s)";
          type = inputDevSubmodule;
          default = { };
        };
      };

      disks = mkOption {
        description = "Disks to format and mount";
        type = types.attrsOf (
          types.submodule {
            options.device = mkOption {
              type = types.str;
              description = ''
                Path to the disk
              '';
            };
          }
        );
        default = { };
        example = literalExpression ''
          {
            disk1.device = "/dev/nvme0n1";
          }
        '';
      };

      network = {
        # TODO? Should add NetVM enabler here?
        # netvm.enable = mkEnableOption = "NetVM";

        pciDevices = mkOption {
          description = "PCI Devices to passthrough to NetVM";
          type = types.listOf pciDevSubmodule;
          default = [ ];
          example = literalExpression ''
            [{
              path = "0000:00:14.3";
              vendorId = "8086";
              productId = "51f1";
            }]
          '';
        };
        kernelConfig = mkOption {
          description = "Hardware specific kernel configuration for network devices";
          type = kernelConfig;
          default = { };
        };
      };

      gpu = {
        # TODO? Should add GuiVM enabler here?
        # guivm.enable = mkEnableOption = "NetVM";

        pciDevices = mkOption {
          description = "PCI Devices to passthrough to GuiVM";
          type = types.listOf pciDevSubmodule;
          default = [ ];
          example = literalExpression ''
            [{
              path = "0000:00:02.0";
              vendorId = "8086";
              productId = "a7a1";
            }]
          '';
        };
        kernelConfig = mkOption {
          description = "Hardware specific kernel configuration for gpu devices";
          type = kernelConfig;
          default = { };
        };
      };

      audio = {
        # With the current implementation, the whole PCI IOMMU group 14:
        #   00:1f.x in the example from Lenovo X1 Carbon
        #   must be defined for passthrough to AudioVM
        pciDevices = mkOption {
          description = "PCI Devices to passthrough to AudioVM";
          type = types.listOf pciDevSubmodule;
          default = [ ];
          example = literalExpression ''
            [
              {
                path = "0000:00:1f.0";
                vendorId = "8086";
                productId = "519d";
              }
              {
                path = "0000:00:1f.3";
                vendorId = "8086";
                productId = "51ca";
              }
              {
                path = "0000:00:1f.4";
                vendorId = "8086";
                productId = "51a3";
              }
              {
                path = "0000:00:1f.5";
                vendorId = "8086";
                productId = "51a4";
              }
            ]
          '';
        };
        kernelConfig = mkOption {
          description = "Hardware specific kernel configuration for audio devices";
          type = kernelConfig;
          default = { };
        };
      };

      usb = {
        internal = mkOption {
          description = ''
            Internal USB device(s) to passthrough.

            Each device definition requires a name, and either vendorId and productId, or hostbus and hostport.
            The latter is useful for addressing devices that may have different vendor and product IDs in the
            same hardware generation.

            Note that internal devices must follow the naming convention to be correctly identified
            and subsequently used. Current special names are:
              - 'cam0' for the internal cam0 device
              - 'fpr0' for the internal fingerprint reader device
          '';
          type = types.listOf usbDevSubmodule;
          default = [ ];
          example = literalExpression ''
            [
              {
                name = "cam0";
                vendorId = "0123";
                productId = "0123";
              }
              {
                name = "fpr0";
                hostbus = "3";
                hostport = "3";
              }
            ]
          '';
        };
        external = mkOption {
          description = "External USB device(s) to passthrough. Requires name, vendorId, and productId.";
          type = types.listOf usbDevSubmodule;
          default = [ ];
          example = literalExpression ''
            [
              {
                name = "external-device-1";
                vendorId = "0123";
                productId = "0123";
              }
              {
                name = "external-device-2";
                vendorId = "0123";
                productId = "0123";
              }
            ]
          '';
        };
      };
    };
}
