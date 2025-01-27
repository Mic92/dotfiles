{ pkgs, ... }:
let
  alfred = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1BA5FRaZOidnPBcW6Ip+4OmZbSbDJkGSMELS3JAI2oOMcZEwlhp0V/hDoBIw2czaMM8oZLBttd+eMgSum/4Dq+TJ/I0wyrY5W3Vni5iO3m3uSdQEWisgsye8D2GziIXN4nE61IcWBjLeVX7eE9FpHu95eWzl/NolSisCqUhqu4TmmGOO7QYKMififJTRh9HL7YBMMylw4aib8MvN6MWdVMHXrzRv+sNE70SHvK2Ioaz1Jxbue2dyvd/Z3xaNox+IWzWZ8mwyvb3E1xNKlZULz3TeBQ+zRne1R3x0NLl+NzIt9hHvfzID79HtTOGdavIav9fBGJEXdv0xHOXXxBG3H devkid@tpinabox"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINypnPSadkRWrby1wscO8F2N7QKYte2bHd+Nhu5VaJ2A work"
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBKbmPTsVBYPtPVtkqHqCbtdPc0BitFLREuLCq36jKXU6cIx7eh0wOZ4p+ihZBT3cU8XPClxWby1cCVBLc+oc/Zs= tablet-samsung-galaxy-tab-s9-fe-biometric"
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOuaxET9xVI5AOOG72qT/4NcQ+/1Oj5+S/E0h/9k+30ZKsOp4aQZR6ETE5S02hzy7Z4Okt/VDpfCwfFRpqdLg8g= phone-google-pixel-7-pro-biometric"
  ];

  alfredsPi = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC/V+n/T0T9UIpkTXqQIcl3Lb4dMUAtu4IIeUxv7rfF9yWHj5VrfEWf3DYDR8xD/OFRmrk3Ab0qu+COEGsc49ejHARKK+ZEL0f4oyh63H41cALwCbz3gjsTPWHAgrdsj27y9vPARC3BLHNwLCZmeuVQOJiXyEHbvt3anf1+XHqPO/KtvohXzKY1dWn4z/AsDWnsht+7wxsRTpYn6tAtcb7asFn/TgD83Vvo2wV0gQVHjsAOu1L6OsE6v1krt308sWuafgAfFyy0bCmiliDszDDyEAcWlPVL7Vxy8JFleerzqHxeCGXRk5Pn1IlzOeGeViFmewbjaiprf3E/FU2yoT5x root@alarmpi";

  alfredsNas = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDPIPMj5mMsKYTkIOw7HPEOVemnRVguP0Q3DZzjcVmefe8f31YOCQCTTkwEWAP+USdW+lNJ1vInpzFvmeUQC1XWqiWEg+b6KqgcvRHRnmXqT5XoV36t3KufJzsfCGYDDwQmWzw9YfdWieJ8i5t3AKFJloUiEXrQ+9PupoOKh86htS3XUL/4x9SMga2ykNGbtrpUfmD0E60BeRZMM089xWKY8hear4RHYSw5iUwHRXdrKlGqc2iTqF69vyzgJqyS1gMRZTk0IQZKuFzz7zOjQ8PsOnU7vZP7SqersaoGmf1WFkauuzu1LuG0aF2xdADmMmd5q4AOd916oEYU871+J7JT root@nas";

  joerg = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine"
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIEVSsc5mlP8aWiUVwWWM3gKlB5LHVpmKSifnDyox/BnVAAAABHNzaDo= yubikey1"
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBCsjXKHCkpQT4LhWIdT0vDM/E/3tw/4KHTQcdJhyqPSH0FnwC8mfP2N9oHYFa2isw538kArd5ZMo5DD1ujL5dLk= ssh@secretive.Joerg’s-Laptop.local"
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
      openssh.authorizedKeys.keys = alfred ++ [
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

  nix.settings.allowed-users = [
    "devkid"
    "shannan"
  ];
}
