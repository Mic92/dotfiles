{ pkgs, ... }:
{
  services.home-assistant.config = {
    sensor = [{
      name = "Random minimix";
      platform = "command_line";
      scan_interval = 60 * 60 * 24;
      command =
        ''${pkgs.curl}/bin/curl -sL https://podcasts.files.bbci.co.uk/p02nrtyg.rss | grep -m1 -oPm1 '(?<=url=")https://[^"]+.mp3' | shuf -n1 '';
    }
      {
        name = "BBC World News";
        platform = "command_line";
        scan_interval = 60 * 60;
        command =
          ''${pkgs.curl}/bin/curl -sL https://www.bbc.co.uk/programmes/p002vsmz/episodes/player | grep -m1 -o -E 'https://www.bbc.co.uk/programmes/w[^"]+' '';
      }];
  };
}
