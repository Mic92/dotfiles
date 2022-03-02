# USAGE: nix repl ./repl.nix --argstr hostname <hostname>
let
  currentHostname = builtins.head (builtins.match "([a-zA-Z0-9]+)\n" (builtins.readFile "/etc/hostname"));
in
  {hostname ? currentHostname}:
    (builtins.getFlake (toString ./..)).nixosConfigurations.${hostname}
