{...}: {
  services.smartd = {
    enable = true;
    devices = [
      { device = "/dev/sda"; }
      { device = "/dev/sdb"; }
    ];
    notifications.mail.enable = true;
    notifications.mail.recipient = "joerg@thalheim.io";
    notifications.test = true;
  };
}
