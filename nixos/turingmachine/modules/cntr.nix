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
    "123456-fsgqa".group = "123456-fsgqa";
  };
  users.groups.fsgqa = { };
  users.groups."123456-fsgqa" = { };
}
