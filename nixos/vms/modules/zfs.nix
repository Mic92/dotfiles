{...}: {
  services.zfs = {
    autoSnapshot.enable = true;
    autoScrub.enable = true;
  };
}
