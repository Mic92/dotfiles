{ ... }:
{
  allowUnfree = true;
  # https://github.com/nix-community/home-manager/issues/2942
  allowUnfreePredicate = _pkg: true;

  pulseaudio = true;
  #allowUnsupportedSystem = true;
  oraclejdk.accept_license = true;
  android_sdk.accept_license = true;

}
