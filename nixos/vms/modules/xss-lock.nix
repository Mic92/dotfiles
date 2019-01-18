{ pkgs, lib, ... }: {
  programs.xss-lock.enable = true;
  programs.xss-lock.lockerCommand = let
    dim-screen = pkgs.writeScript "dim-screen.sh" ''
      #!${pkgs.stdenv.shell}
      export PATH=${lib.getBin pkgs.xlibs.xbacklight}/bin:$PATH
      trap "exit 0" INT TERM
      trap "kill \$(jobs -p); xbacklight -steps 1 -set $(xbacklight -get);" EXIT
      xbacklight -time 5000 -steps 400 -set 0 &
      sleep 2147483647 &
      wait
    '';
  in
    "-n ${dim-screen} -- ${pkgs.i3lock-fancy}/bin/i3lock-fancy";

  systemd.user.services.xss-lock.serviceConfig = {
    ExecStartPre = "${pkgs.xlibs.xset}/bin/xset s 180 120";
  };
}
