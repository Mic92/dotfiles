{
  # NixOS produces many wakeups per second, which is bad for battery life.
  # This kernel parameter disables the timer tick on the last 4 cores
  boot.kernelParams = [ "nohz_full=4-7" ];
}
