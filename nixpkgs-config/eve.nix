{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
  ];
  home.packages = [
    pkgs.profanity
    pkgs.nur.repos.mic92.signald
    (pkgs.weechat.override {
      configure = { availablePlugins, ... }: {
        scripts = with pkgs.weechatScripts; [
          weechat-otr
          wee-slack
          multiline
          pkgs.nur.repos.mic92.weechat-signal
        ];
        plugins = [
          availablePlugins.python
          availablePlugins.perl
          availablePlugins.lua
        ];
      };
    })
  ];
}
