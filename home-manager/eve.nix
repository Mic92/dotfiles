{ pkgs, ... }: {
  imports = [
    ./common.nix
    ./modules/atuin-autosync.nix
  ];
  home.packages =
    let
      weechat = pkgs.wrapWeechat pkgs.weechat-unwrapped { };
    in
    [
      (pkgs.profanity.override {
        # may cause hangs? https://github.com/profanity-im/profanity/pull/1564
        notifySupport = false;
      })
      (weechat.override {
        configure = { availablePlugins, ... }: {
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

      pkgs.matterbridge
    ];
}
