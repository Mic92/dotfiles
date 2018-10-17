{
  services.teamspeak3.enable = true;
  services.netdata.portcheck.checks = {
    teamspeak-ft.port = 30033;
    teamspeak-sq.port = 10011;
  };
}
