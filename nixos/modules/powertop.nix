{
  powerManagement.powertop.enable = true;
  systemd.services.powertop.postStart = ''
    HIDDEVICES=$(ls /sys/bus/usb/drivers/usbhid | grep -oE '^[0-9]+-[0-9\.]+' | sort -u)
    for i in $HIDDEVICES; do
      echo -n "Enabling " | cat - /sys/bus/usb/devices/$i/product
      echo 'on' > /sys/bus/usb/devices/$i/power/control
    done
  '';
  # FIXME always coredumps on boot
  systemd.services.powertop.serviceConfig.Restart = "on-failure";
  systemd.services.powertop.serviceConfig.RestartSec = "2s";
}
