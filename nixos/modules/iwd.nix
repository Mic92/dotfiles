{
  networking.wireless.iwd = {
    enable = true;
    settings = {
      General.AddressRandomization = "once";
      General.AddressRandomizationRange = "full";
    };
  };

  # if network manager is used
  networking.networkmanager.wifi.backend = "iwd";
}
