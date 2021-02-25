{...}: {
  services.zfs = {
    autoSnapshot.enable = true;
    autoSnapshot.monthly = 2;
    autoScrub.enable = true;
  };
}
