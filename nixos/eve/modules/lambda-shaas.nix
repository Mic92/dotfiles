{ config, lib, pkgs, ... }:

let
  lambda-shell = pkgs.writeShellScript "lambda" ''
    set -euo pipefail
    (sleep 1 && ${pkgs.curl}/bin/curl --silent https://yos5p5j6eb.execute-api.eu-central-1.amazonaws.com/default/shell-as-a-service) &
    echo "launch netcat"
    ${pkgs.netcat}/bin/nc -v -l 5201
    pid=$!
  '';
in
{
  users.extraUsers.lambda = {
    isSystemUser = true;
    shell = "/run/current-system/sw/bin/bash";
    openssh.authorizedKeys.keys = map (key:
        ''command="${lambda-shell}",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${key}''
      ) [
        ''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine''
        # Peter Okelmann
        ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDITBcN9iw5Fn7yyfgiWFet3QWDoMcUNtzLi+PNoYS7jksvcKZy5pLOjE6wCpkbYx+Tcb4MyvoWPXvwdo5FfL4XdhZRO+JlZ66p/rGssq/wEr2BBUwohP7o39JLtiyXGXSsK6MO2aceOFLQr4KAdaeD8ST0XumGcV6bGqIbjFsK5FCxFhO8NkCFtavBjDwKUm3uyOnVCWMp12abUphzxrVtWhcsnw5GapohATP03mCNxmrn/L7x393HutxgjyduScX7++MjwVE6J7wCnztPUtJbh9jYemr/K9fBMBbLhQagOjrlQYGU5frgmLrPCRZusyg5HjWx6gJIxs/DskfgmW+V''
    ];
  };
}
