# Workaround for AX210 firmware crash hanging the whole system.
#
# When the AX210 firmware crashes and the device falls off the PCIe bus,
# every iwlwifi register access busy-polls for 15ms while holding RTNL,
# stalling the system for minutes (hung tasks in NetworkManager, syncthing,
# ...) until a hard reboot. See kernel bugzilla 221251, 221178, 220988.
{
  boot.kernelPatches = [
    {
      name = "iwlwifi-mark-transport-dead";
      patch = ./0001-wifi-iwlwifi-pcie-mark-transport-dead-when-device-fa.patch;
    }
    {
      name = "iwlwifi-dead-device-debug-logging";
      patch = ./0002-wifi-iwlwifi-pcie-add-debug-logging-for-dead-device-.patch;
    }
  ];

  # Schedule PCIe remove/rescan when the device is gone, so wifi can
  # recover without a reboot.
  boot.extraModprobeConfig = ''
    options iwlwifi remove_when_gone=1
  '';
}
