{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
  ];
  home.packages = [
    pkgs.profanity
    (pkgs.weechat.override {
      configure = { availablePlugins, ... }: {
        scripts = with pkgs.weechatScripts; [
          weechat-otr
          wee-slack
          multiline
          weechat-matrix
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
