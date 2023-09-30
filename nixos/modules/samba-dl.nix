{
  fileSystems."/data/torrent/download/prism" = {
    device = "//prism.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
      "rsize=16777216"
      "cache=loose"
    ];
  };
  #fileSystems."/data/torrent/download/catalonia" = {
  #  device = "//mukke.r/public";
  #  fsType = "cifs";
  #  options = [
  #    "guest"
  #    "nofail"
  #    "noauto"
  #    "ro"
  #    "rsize=16777216"
  #    "cache=loose"
  #  ];
  #};
}
