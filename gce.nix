let
  credentials = {
    project = "evaa-163222";
    serviceAccount = "718643087322-compute@developer.gserviceaccount.com";
    accessKey = "/etc/nixos/secrets/eva.pem";
    region = "us-east1-d";
  };

in {
  network.description = "Monitoring server";

  resources.gceStaticIPs.eva-ip = credentials // {
    region = "us-east1";
  };
  eva = { resources, lib, ... }: {
    deployment.targetEnv = "gce";
    deployment.gce = credentials // {
      instanceType = "f1-micro";
      rootDiskSize = 29;
      ipAddress = resources.gceStaticIPs.eva-ip;
    };
  };
}
