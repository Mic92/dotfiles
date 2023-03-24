{ pkgs, ... }:

let
  screenshareWebsite = roomName: {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    root = pkgs.runCommand "screenshare.thalheim.io" { } ''
      mkdir -p $out
      sed -e 's/@roomName@/${roomName}/g' ${./index.html} > $out/index.html
    '';
    extraConfig = ''
      charset utf-8;
      source_charset utf-8;
    '';
  };
in
{
  services.nginx = {
    virtualHosts."screenshare.thalheim.io" = screenshareWebsite "screenshare";
    virtualHosts."screenshare-numtide.thalheim.io" = screenshareWebsite "numtide";
    virtualHosts."screenshare2.thalheim.io" = screenshareWebsite "screenshare2";

    virtualHosts."www.screenshare.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "screenshare.thalheim.io";
    };

    virtualHosts."www.screenshare2.thalheim.io" = {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      globalRedirect = "screenshare2.thalheim.io";
    };
  };
}
