let
  region = "us-east-1";
  zone = "us-east-1a";
  accessKeyId = "default";
in {
  resources.ebsVolumes.cntr-disk = {
    accessKeyId = accessKeyId;
    tags.Name = "cntr-disk";
    inherit region;
    zone = zone;
    size = 100;
    volumeType = "gp2";
  };

  cntr-machine = { resources, config, lib, pkgs, ... }: {
    deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.zone = zone;
    deployment.ec2.instanceType = "m4.xlarge";
    deployment.ec2.associatePublicIpAddress = true;
    deployment.ec2.keyPair = resources.ec2KeyPairs.cntr-key-pair;
    deployment.ec2.elasticIPv4 = resources.elasticIPs.cntr-ip6;
    deployment.ec2.ebsInitialRootDiskSize = 30;
    deployment.ec2.subnetId = resources.vpcSubnets.cntr-subnet;
    deployment.ec2.securityGroupIds = [ resources.ec2SecurityGroups.cntr-security-group.name ];

    imports = [
      ./modules/users.nix
      ./modules/tracing.nix
      ./modules/packages.nix
    ];

    services.nfs.server = {
      enable = true;
      exports = "/mnt/original 127.0.0.1(rw,no_root_squash)";
    };

    #fileSystems."/scratch" = {
    #  fsType = "nfs";
    #  device = "127.0.0.1:/mnt/original";
    #};

    boot.kernelPackages = pkgs.linuxPackages_latest;

    fileSystems."/mnt/original" = {
      autoFormat = true;
      fsType = "ext4";
      device = "/dev/xvdj";
      ec2.disk = resources.ebsVolumes.cntr-disk;
    };

    #fileSystems."/xfstests-scratch" = {
    #  autoFormat = true;
    #  fsType = "ext4";
    #  device = "/dev/xvdk";
    #  ec2.disk = resources.ebsVolumes.cntr-disk3;
    #};

    #fileSystems."/xfstests-test" = {
    #  autoFormat = true;
    #  fsType = "ext4";
    #  device = "/dev/xvdl";
    #  ec2.disk = resources.ebsVolumes.cntr-disk4;
    #};
  };

  resources.ec2KeyPairs.cntr-key-pair = { inherit region accessKeyId; };

  resources.ec2SecurityGroups.cntr-security-group = { resources, ... }: {
    inherit region accessKeyId;
    vpcId = resources.vpc.cntr-vpc;
    rules = [
      { protocol = "-1"; fromPort = 0; toPort = 65535; sourceIp = "0.0.0.0/0"; }
      #{ protocol = "-1"; fromPort = 0; toPort = 65535; sourceIp = "::/0"; }
    ];
  };

  resources.vpc.cntr-vpc = {
    inherit region accessKeyId;
    instanceTenancy = "default";
    enableDnsSupport = true;
    enableDnsHostnames = true;
    cidrBlock = "192.168.56.0/24";
    amazonProvidedIpv6CidrBlock = true;
  };

  resources.vpcSubnets.cntr-subnet = { resources, ... }: {
    inherit region zone accessKeyId;
    vpcId = resources.vpc.cntr-vpc;
    cidrBlock = "192.168.56.0/25";
    mapPublicIpOnLaunch = true;
    tags.Source = "NixOps";
  };

  resources.elasticIPs.cntr-ip6 = {
    inherit region accessKeyId;
    vpc = true;
  };

  resources.vpcInternetGateways.cntr-igw = { resources, ... }: {
    inherit region accessKeyId;
    vpcId = resources.vpc.cntr-vpc;
  };

  resources.vpcRouteTables.cntr-route-table =  { resources, ... }: {
    inherit region accessKeyId;
    vpcId = resources.vpc.cntr-vpc;
  };

  resources.vpcRouteTableAssociations.cntr-association = { resources, ... }: {
    inherit region accessKeyId;
    subnetId = resources.vpcSubnets.cntr-subnet;
    routeTableId = resources.vpcRouteTables.cntr-route-table;
  };

  resources.vpcRoutes.cntr-igw-route = { resources, ... }: {
    inherit region accessKeyId;
    routeTableId = resources.vpcRouteTables.cntr-route-table;
    destinationCidrBlock = "0.0.0.0/0";
    gatewayId = resources.vpcInternetGateways.cntr-igw;
  };

  resources.vpcRoutes.cntr-igw-route6 = { resources, ... }: {
    inherit region accessKeyId;
    routeTableId = resources.vpcRouteTables.cntr-route-table;
    destinationIpv6CidrBlock = "::/0";
    gatewayId = resources.vpcInternetGateways.cntr-igw;
  };
}
