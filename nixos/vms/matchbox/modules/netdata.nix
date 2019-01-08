{ ... }: {
  imports = [ ../../ ];

  services.netdata.config.global = {
    "bind to" = "0.0.0.0:19999 [::]:19999";
    "access log" = "stdout";
    "error log" = "stderr";
    "error log" = "stderr";
    "update every" = "5";
    "memory mode" = "ram";
  };
}
