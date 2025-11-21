{ pkgs, ... }:
{
  imports = [
    ./common.nix
    ./modules/ai.nix
    ./modules/atuin-autosync.nix
  ];
  home.packages =
    let
      weechat = pkgs.wrapWeechat pkgs.weechat-unwrapped { };
    in
    [
      (weechat.override {
        configure =
          { availablePlugins, ... }:
          {
            scripts = with pkgs.weechatScripts; [
              wee-slack
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
