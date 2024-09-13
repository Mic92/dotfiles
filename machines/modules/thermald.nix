{ config, ... }:
{
  services.thermald.enable = config.hardware.cpu.intel.updateMicrocode;
}
