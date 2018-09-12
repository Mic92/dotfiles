{ ... }: {
  services.kresd.enable = true;
  services.kresd.interfaces = [ "::" ];
}
