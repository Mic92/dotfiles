{ pkgs, ... }:
let
  ps4Cmd = command: ''
    ${pkgs.openssh}/bin/ssh -o "StrictHostKeyChecking no" light@turingmachine.r "pyps4-2ndscreen ${command}"
  '';
in {
  services.home-assistant.config = {
    shell_command.ps4_start_netflix = ps4Cmd "start CUSA00127";
    intent_script.StartNetflix = {
      speech.text = "Started Netflix.";
      action.service = "shell_command.ps4_start_netflix";
    };

    shell_command.ps4_start_nowtv = ps4Cmd "start CUSA00117";
    intent_script.StartNowTV = {
      speech.text = "Started NowTV.";
      action.service = "shell_command.ps4_start_nowtv";
    };

    shell_command.ps4_start_all4 = ps4Cmd "start CUSA00072";
    intent_script.StartAll4 = {
      speech.text = "Started All4.";
      action.service = "shell_command.ps4_start_all4";
    };

    shell_command.ps4_start_youtube = ps4Cmd "start CUSA01116";
    intent_script.StartYoutube = {
      speech.text = "Started Youtube.";
      action.service = "shell_command.ps4_start_youtube";
    };

    shell_command.ps4_standby = ps4Cmd "standby";
    intent_script.StandbyPS4 = {
      speech.text = "Put PS4 to sleep.";
      action.service = "shell_command.ps4_standby";
    };
  };
}
