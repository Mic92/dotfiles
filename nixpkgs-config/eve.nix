{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
  ];
  home.packages = [
    (pkgs.weechat.override {
      configure = { availablePlugins, ... }: {
        scripts = with pkgs.weechatScripts; [ weechat-otr wee-slack ];
        plugins = [
          availablePlugins.python
          availablePlugins.perl
          availablePlugins.lua
        ];
      };
    })
  ];
}
