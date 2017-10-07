let
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine";
  legacyKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDGoQXNL3B1+pS4WYfhvn4ULb6oCNovT+dpWist7osToj5UVQ64odlcemnSG07GRcEnwf2zDTYq8eatomGQ94VsnmWuKaYzF8nqNl+qHRM49nS+Myi2ETn0B5fnMSh45lmkjR5rL/tb02EXUVoNf7acE2K3Q8M/tGFEdCdQNuqEgishi5nrs/WvZHn0cxP1anv8WRtm2qlj0jtH1rYmo7n/xsPb15FNBaE92aQXTkGoj6xdQknGWnGjLLm33lGIxRKvHTJ9T2NGte4gTYC/CADPxU2x5nq8zGDTNna/YMUyKmlqgGm+p+sE9dERmxKtquLgyE8mNvjDSMvtnrkMojN5 joerg@turingmachine";
in {
  users.extraUsers = {
    joerg = {
      isNormalUser = true;
      home = "/home/joerg";
      extraGroups = [ "wheel" "docker" "plugdev" "vboxusers" "adbusers" "input" ];
      shell = "/run/current-system/sw/bin/zsh";
      uid = 1000;
      openssh.authorizedKeys.keys = [ key ];
    };

    root.openssh.authorizedKeys.keys = [ key ];
  };

  boot.initrd.network.ssh.authorizedKeys = [ legacyKey ];

  security.sudo.wheelNeedsPassword = false;
  programs.zsh.enable = true;
}
