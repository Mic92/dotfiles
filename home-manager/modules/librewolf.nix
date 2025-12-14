{
  pkgs,
  lib,
  ...
}:

let
  firefoxExtensions = pkgs.callPackages ../../pkgs/firefox-extensions { };

  # Firefox application GUID
  firefoxGuid = "{ec8030f7-c20a-464f-9b0e-13a3a9e97384}";

  # Extension IDs
  extensions = [
    {
      pkg = firefoxExtensions.browser-cli-extension;
      id = "browser-cli-controller@thalheim.io";
    }
    {
      pkg = firefoxExtensions.chrome-tab-gc-extension;
      id = "tab-garbage-collector@thalheim.io";
    }
  ];

  # Generate home.file entries for sideloading extensions as XPI files
  # Firefox 62+ requires packed XPI files, not unpacked directories
  # On Linux: ~/.librewolf/extensions/{firefox-guid}/{extension-id}.xpi
  # On macOS: ~/Library/Application Support/LibreWolf/Extensions/{firefox-guid}/{extension-id}.xpi
  extensionFiles = builtins.listToAttrs (
    map (ext: {
      name =
        if pkgs.stdenv.isDarwin then
          "Library/Application Support/LibreWolf/Extensions/${firefoxGuid}/${ext.id}.xpi"
        else
          ".librewolf/extensions/${firefoxGuid}/${ext.id}.xpi";
      value = {
        source = "${ext.pkg}/${ext.pkg.pname}.xpi";
      };
    }) extensions
  );
in
{
  # LibreWolf is installed via darwinModules/nix-casks.nix on macOS
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.librewolf ];

  # xxx, this doesn't work automatically yet, but loading from a file works....
  home.file = extensionFiles;
}
