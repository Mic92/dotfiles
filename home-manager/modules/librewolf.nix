{
  pkgs,
  lib,
  inputs,
  ...
}:

let
  firefoxExtensions = pkgs.callPackages ../../pkgs/firefox-extensions { };
  micsSkillsPkgs = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};

  librewolfPath =
    if pkgs.stdenv.hostPlatform.isDarwin then
      "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf"
    else
      "${pkgs.librewolf}/bin/librewolf";
in
{
  # LibreWolf is installed via darwinModules/nix-casks.nix on macOS
  home.packages = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    (pkgs.librewolf.override {
      extraPolicies = import ../../pkgs/librewolf-policies.nix {
        inherit (pkgs) lib stdenv;
        inherit (micsSkillsPkgs) browser-cli-extension;
        inherit (firefoxExtensions) chrome-tab-gc-extension app-windows-extension;
      };
    })
  ];

  # Tell browser-cli where LibreWolf is so browsh can find it
  xdg.configFile."browser-cli/config.toml".text = ''
    firefox_path = "${librewolfPath}"
  '';

  # noctalia is its own pywalfox host but only registers it under
  # ~/.mozilla. Resolve it via the system profile so desktops without
  # niri/noctalia don't pull it in; the addon just fails to connect there.
  home.file.".librewolf/native-messaging-hosts/pywalfox.json" =
    lib.mkIf pkgs.stdenv.hostPlatform.isLinux
      {
        text = builtins.toJSON {
          name = "pywalfox";
          description = "Noctalia Firefox theme native messaging host";
          path = "/run/current-system/sw/bin/noctalia";
          type = "stdio";
          allowed_extensions = [ "pywalfox@frewacom.org" ];
        };
      };

  # Register native messaging host so the browser extension can talk to browser-cli
  home.activation.installBrowserCliHost = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ${micsSkillsPkgs.browser-cli}/bin/browser-cli --install-host
  '';
}
