{ pkgs, ...}:
{
  hardware.opengl = {
    enable = true;
    extraPackages = [ pkgs.vaapiIntel ];
    driSupport32Bit = true;
  };
}
