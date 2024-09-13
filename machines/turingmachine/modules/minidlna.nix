{
  networking.firewall.allowedTCPPorts = [ 8200 ];
  services.minidlna = {
    enable = true;
    loglevel = "debug";
    mediaDirs = [ "V,/var/lib/minidlna" ];
    friendlyName = "turingmachine";
    rootContainer = "V";
    extraConfig = ''
      album_art_names=Cover.jpg/cover.jpg/AlbumArtSmall.jpg/albumartsmall.jpg
      album_art_names=AlbumArt.jpg/albumart.jpg/Album.jpg/album.jpg
      album_art_names=Folder.jpg/folder.jpg/Thumb.jpg/thumb.jpg
      notify_interval=60
    '';
  };
}
