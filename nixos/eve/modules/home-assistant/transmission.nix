{ config, lib, pkgs, ... }:

{
  services.home-assistant.config = {
    transmission.host = "yellow.r";
    automation = [{
      alias = "Completed Torrent";
      trigger = {
        platform = "event";
        event_type = "transmission_downloaded_torrent";
      };
      action = {
        service = "notify.irc_flix";
        data = {
          message = "torrent completed: {{trigger.event.data.name}}";
        };
      };
    }];
    notify = [{
      name = "irc_flix";
      platform = "command_line";
      command = ''
        ${pkgs.nur.repos.mic92.irc-announce}/bin/irc-announce irc.r 6667 transmission '#flix' "$(cat)"
      '';
    }];
  };
}
