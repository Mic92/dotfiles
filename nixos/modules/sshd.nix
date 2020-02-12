{
  services.openssh = {
    enable = true;
    startWhenNeeded = true;
    passwordAuthentication = false;
    useDns = false;
  };
}
