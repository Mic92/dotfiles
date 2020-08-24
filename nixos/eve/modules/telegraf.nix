{
  imports = [
    ../../modules/telegraf.nix
  ];

  services.telegraf.extraConfig.inputs.postfix = {};
}
