{
  self,
  inputs,
  pkgs,
  config,
  ...
}:
{
  networking.hostName = "evo";
  nixpkgs.hostPlatform = "aarch64-darwin";

  imports = [
    inputs.srvos.darwinModules.common
    inputs.srvos.darwinModules.mixins-telegraf
    inputs.srvos.darwinModules.mixins-terminfo
    inputs.srvos.darwinModules.mixins-nix-experimental
    inputs.sops-nix.darwinModules.sops
    ../../darwinModules/app-store
    ../../darwinModules/clan/default.nix
    ../../darwinModules/homebrew.nix
    ../../darwinModules/iterm2.nix
    ../../darwinModules/nix-daemon.nix
    ../../darwinModules/nix-index.nix
    ../../darwinModules/openssh.nix
    ../../darwinModules/remote-builder.nix
    ../../darwinModules/secretiv.nix
    ../../darwinModules/sudo.nix
    ../../darwinModules/ghostty.nix
  ];

  system.activationScripts.postActivation.text = ''
    # disable spotlight
    launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist >/dev/null 2>&1 || true
    # disable fseventsd on /nix volume
    mkdir -p /nix/.fseventsd
    test -e /nix/.fseventsd/no_log || touch /nix/.fseventsd/no_log
  '';

  clan.core.settings.directory = ../..;
  clan.core.settings.machine.name = "evo";

  sops.age.keyFile = "/Library/Application Support/sops-nix/age-key.txt";

  sops.secrets.test-secret = {
    owner = "joerg";
    path = "${config.users.users.joerg.home}/.foo";
    sopsFile = ./test-secrets.yml;
  };
  sops.templates."test-template.toml" = {
    content = ''
      password = "${config.sops.placeholder.test-secret}";
    '';
    uid = 501;
  };

  # fix vim repeat key
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  users.users.joerg.home = "/Users/joerg";

  environment.etc."nix-darwin".source = "${config.users.users.joerg.home}/.homesick/repos/dotfiles";

  environment.systemPackages = [ pkgs.python3 ];

  programs.zsh.enable = true;

  system.stateVersion = 5;

  srvos.flake = self;
}
