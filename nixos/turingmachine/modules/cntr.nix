{ pkgs, ... }:
{
  virtualisation.oci-containers = {
    containers.nginx = {
      image = "nginx-container";
      imageFile = pkgs.dockerTools.examples.nginx;
    };
  };

  # fstest
  users.users = {
    fsgqa = {
      isNormalUser = false;
      group = "fsgqa";
    };
    fsgqa2 = {
      isNormalUser = false;
      group = "fsgqa";
    };
    "123456-fsgqa" = {};
  };
  users.groups.fsgqa = {};
}
