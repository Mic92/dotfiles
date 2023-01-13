{ nurFun ? (import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz")), ... }: {
  allowUnfree = true;
  # https://github.com/nix-community/home-manager/issues/2942
  allowUnfreePredicate = _pkg: true;

  pulseaudio = true;
  #allowUnsupportedSystem = true;
  oraclejdk.accept_license = true;
  android_sdk.accept_license = true;

  packageOverrides = pkgs: {
    nur = nurFun {
      nurpkgs = pkgs;
      inherit pkgs;
    };
  };
}
