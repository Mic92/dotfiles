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
    # FIXME: upgrade to python310 when possible
    python3Packages = pkgs.python38Packages;
    weechat-unwrapped = pkgs.weechat-unwrapped.override {
      inherit python3Packages;
    };
    weechatScripts = pkgs.weechatScripts.override {
      inherit python3Packages;
    };
    weechat = pkgs.wrapWeechat weechat-unwrapped {};
  in [
    pkgs.profanity
    (weechat.override {
      configure = {availablePlugins, ...}: {
        scripts = with weechatScripts; [
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

    # for development
    pkgs.kubectl
    pkgs.kotlin-language-server
  ];
}
