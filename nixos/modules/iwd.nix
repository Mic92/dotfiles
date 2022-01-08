{ ... }: {
  networking.wireless.iwd = {
    enable = true;
    settings = {
      General.AddressRandomization = "once";
      General.AddressRandomizationRange = "full";
    };
  };
}
