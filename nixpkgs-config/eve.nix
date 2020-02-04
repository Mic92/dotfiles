{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
  ];
  home.packages = with pkgs; [
    (weechat.override {
      configure = { availablePlugins, ... }: {
        scripts = with weechatScripts; [ weechat-otr wee-slack ];
        plugins = [
          availablePlugins.python
          availablePlugins.perl
          availablePlugins.lua
        ];
      };
    })
  ];
}
