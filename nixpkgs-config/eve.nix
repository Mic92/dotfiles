{ pkgs, lib, config, ... }:

{
  imports = [
    ./common.nix
  ];
  home.packages = let
    # FIXME: upgrade to python310 when possible
    python3Packages = pkgs.python3Packages;
    unwrapped-weechat = pkgs.weechat-unwrapped.override {
      inherit python3Packages;
    };
    weechatScripts = pkgs.weechatScripts.override {
      inherit python3Packages;
    };
    weechat = pkgs.wrapWeechat unwrapped-weechat {};
  in [
    pkgs.profanity
    (weechat.override {
      configure = { availablePlugins, ... }: {
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
  ];
}
