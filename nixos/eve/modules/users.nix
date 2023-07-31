{ pkgs, ... }:
let
  alfred = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzdS6xog803ySrz1+hTUYlL89Wbb5p+7hd1WvDXHP4ERICuouVYO/F54saCokpcZBSyMtBC11+Yvk5J+L6pNuDJki04y4fr0HMmIVc5khuvNAiiH/8IFZk9v8uf7dyHVJyKIB+4LFMXuFB5i9gtoTM8WpIu8lYzIK6BEG1xhnfmPrLTWOw4w1Ty3iE93VPt3qRYxsB6Dx4f2n3S0piLQ+sX/aHiDO+MNdZTKJMdzPkqp89b8kF6vRyAp8WuiQDJkZJK+QKG+dvMKAofv7G97eO01TKNLPLqtswDGCnkXjkBrQ2tY7Nq5fannLGKBl+qOu3SRq8FRBaiPDa7uzCV3Vr devkid@desktop"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1BA5FRaZOidnPBcW6Ip+4OmZbSbDJkGSMELS3JAI2oOMcZEwlhp0V/hDoBIw2czaMM8oZLBttd+eMgSum/4Dq+TJ/I0wyrY5W3Vni5iO3m3uSdQEWisgsye8D2GziIXN4nE61IcWBjLeVX7eE9FpHu95eWzl/NolSisCqUhqu4TmmGOO7QYKMififJTRh9HL7YBMMylw4aib8MvN6MWdVMHXrzRv+sNE70SHvK2Ioaz1Jxbue2dyvd/Z3xaNox+IWzWZ8mwyvb3E1xNKlZULz3TeBQ+zRne1R3x0NLl+NzIt9hHvfzID79HtTOGdavIav9fBGJEXdv0xHOXXxBG3H devkid@tpinabox"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDfp9es60l+4L2yJ4O9LS3R2L4EcSdrqGeZBaTCzhgpEw+ptUzlQsmqUMSC+FtfXRE7jhMsD2ayNfSgVLGVy0Kd91T3fYPeOC4ibrYEbfnJGnnretHe2CdPESN6nyNzI2lg4WVZbuuZh+nV96839ORqVcEESDtq7Le1G9cFwP0w6hBnzNukiWv+bwvZYu1eUDevgnnW6bcwNbr+S4NF9Ddji17ZYhld5l0y0vrgZzYmeOLE7bjL3iwLp3/SzHd+K5+xOMLIfrUIRyJs39aCuesAUXaht9I91UBU5ML77NfpF5L4VIn3613VYfJV6kV17uHU0Rvgh5q6YK1fpq1+M7m7 alfred@alfred-HP-Compaq-Elite-8300-MT"
  ];

  alfredsPi = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC/V+n/T0T9UIpkTXqQIcl3Lb4dMUAtu4IIeUxv7rfF9yWHj5VrfEWf3DYDR8xD/OFRmrk3Ab0qu+COEGsc49ejHARKK+ZEL0f4oyh63H41cALwCbz3gjsTPWHAgrdsj27y9vPARC3BLHNwLCZmeuVQOJiXyEHbvt3anf1+XHqPO/KtvohXzKY1dWn4z/AsDWnsht+7wxsRTpYn6tAtcb7asFn/TgD83Vvo2wV0gQVHjsAOu1L6OsE6v1krt308sWuafgAfFyy0bCmiliDszDDyEAcWlPVL7Vxy8JFleerzqHxeCGXRk5Pn1IlzOeGeViFmewbjaiprf3E/FU2yoT5x root@alarmpi";

  alfredsNas = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDPIPMj5mMsKYTkIOw7HPEOVemnRVguP0Q3DZzjcVmefe8f31YOCQCTTkwEWAP+USdW+lNJ1vInpzFvmeUQC1XWqiWEg+b6KqgcvRHRnmXqT5XoV36t3KufJzsfCGYDDwQmWzw9YfdWieJ8i5t3AKFJloUiEXrQ+9PupoOKh86htS3XUL/4x9SMga2ykNGbtrpUfmD0E60BeRZMM089xWKY8hear4RHYSw5iUwHRXdrKlGqc2iTqF69vyzgJqyS1gMRZTk0IQZKuFzz7zOjQ8PsOnU7vZP7SqersaoGmf1WFkauuzu1LuG0aF2xdADmMmd5q4AOd916oEYU871+J7JT root@nas";

  joerg = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDGoQXNL3B1+pS4WYfhvn4ULb6oCNovT+dpWist7osToj5UVQ64odlcemnSG07GRcEnwf2zDTYq8eatomGQ94VsnmWuKaYzF8nqNl+qHRM49nS+Myi2ETn0B5fnMSh45lmkjR5rL/tb02EXUVoNf7acE2K3Q8M/tGFEdCdQNuqEgishi5nrs/WvZHn0cxP1anv8WRtm2qlj0jtH1rYmo7n/xsPb15FNBaE92aQXTkGoj6xdQknGWnGjLLm33lGIxRKvHTJ9T2NGte4gTYC/CADPxU2x5nq8zGDTNna/YMUyKmlqgGm+p+sE9dERmxKtquLgyE8mNvjDSMvtnrkMojN5 joerg@turingmachine"
  ];

  shannan = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOBXTForyB6oNMK5bbGpALKU4lPsKRGxNLhrE/PnHHq7 shannan@bernie"
  ];

in
{
  security.sudo.wheelNeedsPassword = false;

  users.users = {
    devkid = {
      isNormalUser = true;
      uid = 2002;
      extraGroups = [ "wheel" ];
      shell = "/run/current-system/sw/bin/zsh";
      openssh.authorizedKeys.keys =
        alfred
        ++ [
          ''command="${pkgs.borgbackup}/bin/borg serve --restrict-to-path /data/backup/devkid/pi0",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${alfredsPi}''
          ''command="${pkgs.borgbackup}/bin/borg serve --restrict-to-path /data/backup/devkid/nas",no-pty,no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-user-rc ${alfredsNas}''
        ];
    };

    joerg = {
      isNormalUser = true;
      uid = 2003;
      extraGroups = [ "wheel" ];
      shell = "/run/current-system/sw/bin/zsh";
      openssh.authorizedKeys.keys = joerg;
    };

    shannan = {
      isNormalUser = true;
      uid = 2005;
      extraGroups = [ "wheel" ];
      shell = "/run/current-system/sw/bin/zsh";
      openssh.authorizedKeys.keys = shannan;
    };

    root.openssh.authorizedKeys.keys = alfred ++ joerg ++ shannan;
  };

  nix.settings.allowed-users = [ "devkid" "shannan" ];
}
