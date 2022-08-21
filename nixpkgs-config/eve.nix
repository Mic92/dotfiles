{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./common.nix
  ];
  home.packages = let
    weechat = pkgs.wrapWeechat pkgs.weechat-unwrapped {};
  in [
    pkgs.profanity
    (weechat.override {
      configure = {availablePlugins, ...}: {
        scripts = with pkgs.weechatScripts; [
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

    # for development
    pkgs.kubectl
    pkgs.kotlin-language-server
    pkgs.yq-go
  ];
}
