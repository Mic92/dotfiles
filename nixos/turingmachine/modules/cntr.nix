{ pkgs, ... }:
{
  #virtualisation.oci-containers = {
  #  containers.nginx = {
  #    image = "nginx-container";
  #    imageFile = pkgs.dockerTools.examples.nginx;
  #  };
  #};

  # fstest
  users.users = {
    fsgqa = {
      isSystemUser = true;
      group = "fsgqa";
    };
    fsgqa2 = {
      isSystemUser = true;
      group = "fsgqa";
    };
    "123456-fsgqa".isSystemUser = true;
  };
  users.groups.fsgqa = { };
}
