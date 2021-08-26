{
  fileSystems."/mnt/prism" = {
    device = "//prism.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
    ];
  };
}
