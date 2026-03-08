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
    if pkgs.stdenv.isDarwin then
      "/Applications/Nix Casks/LibreWolf.app/Contents/MacOS/librewolf"
    else
      "${pkgs.librewolf}/bin/librewolf";
in
{
  # LibreWolf is installed via darwinModules/nix-casks.nix on macOS
  home.packages = lib.optionals pkgs.stdenv.isLinux [
    (pkgs.librewolf.override {
      extraPolicies = import ../../pkgs/librewolf-policies.nix {
        inherit (micsSkillsPkgs) browser-cli-extension;
        inherit (firefoxExtensions) chrome-tab-gc-extension;
      };
    })
  ];

  # Tell browser-cli where LibreWolf is so browsh can find it
  xdg.configFile."browser-cli/config.toml".text = ''
    firefox_path = "${librewolfPath}"
  '';

  # Register native messaging host so the browser extension can talk to browser-cli
  home.activation.installBrowserCliHost = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ${micsSkillsPkgs.browser-cli}/bin/browser-cli --install-host
  '';
}
