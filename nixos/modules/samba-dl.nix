{
  fileSystems."/data/torrent/download/prism" = {
    device = "//prism.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
    ];
  };
  fileSystems."/data/torrent/download/catalonia" = {
    device = "//catalonia.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
    ];
  };
}
