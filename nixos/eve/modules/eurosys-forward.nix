{
  services.openssh = {
    extraConfig = ''
      Match User reviewer
        PermitTTY no
        X11Forwarding no
        PermitTunnel no
        GatewayPorts no
    '';
  };
  users.users.reviewer = {
    isSystemUser = true;
    group = "reviewer";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOCNKi5/EwjJhW827IuCOmPI7AclIr0QmH7q3SSwj8MF aec-key"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCv+Mt+wQuD3hcsOSCgbIb2n0gKSqT9R4OQ/j5iClZaYAC6uzlYMQ7TCcXXV4kvG4A1UFpBFIxbcNc7fuLCump2Umezzl8kl5IoTFxTU7GgRruyhxYDAglnE1HobX83qtlkzaMQ4U9xVphik3T/EtbQRWABrIHwMKyJtpcWLyKPginQkt+AelXQ6M5Rw5dOhVV+gSLkIbbc8Rnu3CNCurkMfkJclppWIWjvXDT0OoUzlSfBgrBIhGOXc+/wR+lcyU6TJObX08z7B3M/afXPtBMNK75J5WR/XQG6TKWmC7fZgsIlnSBFEULFlmrrUIKp89IRbpryOnHtowei71h9pxfj mark-lee@mac-air"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICkWqC4QyrcoJoZzQfk3agvQyZBeO/+Rq2PES+gLhNpe eurosys-aec"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCqEBnN7W8bfQIafORn4JpIjNXb81T1A8QuHw/JC2seu5jWwYiPci5E8my3JuPBa7BLlVUenBCfXJvI5ixKnlk4SZ/MTRmrnPKVCkqZqbJihAVh6tMb2PHXP4cxgG26TWMYKJjoIr3Mkq3KTv73ntawItZHLUkNg66amTtN2xryEroBB59rB6WIYQ9UEDXoejNuWDY4BGynYU5niY4FpCG0SVSXwK2JCRfBGCHLJHpZwIImQIayvbVQDv58Q+lFt6JMSmEavCAYgpWyYHGvO99jy59i7DEqxmW99PiOW9GnwtH7UovB9593Ij2UcD2She+ZyjWfWIpeyZA4Z/SaqjApENImO8cLtwHyHAYo2y2tFkHET0cQlEaIcj8OmZn8KXO7dpnpeXFRjV43XyFE34s5qO29uaLKhBQZHauz9SnvySUMxbiprml0CjEoZm4EjMNJSPxndBc5b2b8fYO/BvuR7a1gUvFX0FxV1Kwdg3t3jm13IbP1aaHOeSjmT73tfUM="

      # my own key for testing
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDITBcN9iw5Fn7yyfgiWFet3QWDoMcUNtzLi+PNoYS7jksvcKZy5pLOjE6wCpkbYx+Tcb4MyvoWPXvwdo5FfL4XdhZRO+JlZ66p/rGssq/wEr2BBUwohP7o39JLtiyXGXSsK6MO2aceOFLQr4KAdaeD8ST0XumGcV6bGqIbjFsK5FCxFhO8NkCFtavBjDwKUm3uyOnVCWMp12abUphzxrVtWhcsnw5GapohATP03mCNxmrn/L7x393HutxgjyduScX7++MjwVE6J7wCnztPUtJbh9jYemr/K9fBMBbLhQagOjrlQYGU5frgmLrPCRZusyg5HjWx6gJIxs/DskfgmW+V"
    ];
  };

  networking.extraHosts = ''
    42:0:3c46:5466:7dbe:f27a:673f:ea64 vmsh
    42:0:3c46:a85d:33c6:14ca:3cf7:8827 lambdapirate
  '';
}
