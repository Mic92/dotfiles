{
  users.extraUsers.sshjump = {
    shell = "/run/current-system/sw/bin/bash";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDoi3DBYWIyvXOjbhxPesfTYHe7++qSE9Ynq6En/jpUUAo6r2RF8mD2MHImRQ7D0NP15cQ1t9HcZQqeWPynAupftW7ECXKpo5eFm9mDWc/bhWHU2OSsgMSOzHaHNw9p8cqw2vwEShJDRfXcnXRk+Eue5Yj3FuJvImbkSxRQeoLEZC+apDvxxz6YdJrkaCDXsSqcjGq84Yp6EYV23/Jr4vHeHjRwjIUdVd1aGs/0j95f/zBrFrqa2WV5tCTl1bIyiJoK4dfy5RLzJCZva9k+qmA24AGQgfXdZl870LARweg+9vdxSzaMJgaKSZ4hYfjXNAj42kKxM5jjeptQvhhzZ+7cKbDnJFl+kEH1psQEaQ2xh4tLcQ7KSWW77voizBUujYA//Lbekaia1Js7jEl76Mp8FU/+5TWFkDoXypHu9z9JxEX2dz1ylEAQXfpyzZSzKLMz/VX++QmYt5CpbpsvfXdnLR+VCMBh2xb3mUEZ6p3fXJUmtvmHgnwvb8Yx3vgjvYs= joerg@turingmachine"
    ];
  };

  services.openssh.extraConfig = ''
    Match User sshjump
      AllowAgentForwarding no
      AllowTcpForwarding yes
      X11Forwarding no
      PermitTunnel no
      GatewayPorts no
      ForceCommand echo 'This account can only be used for ProxyJump (ssh -J)'
    Match all
  '';

}
