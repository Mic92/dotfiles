{
  fileSystems."/data/torrent/download/prism" = {
    device = "//yellow.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
      "rsize=16777216"
      "cache=loose"
      "x-systemd.after=network.target"
    ];
  };

  fileSystems."/data/torrent/download/catalonia" = {
    device = "//fd98:c3d0:bec8::2/tonne";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
      "rsize=16777216"
      "cache=loose"
      "x-systemd.after=network.target"
    ];
  };
}
