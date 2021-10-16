{ mkShell, sops-import-keys-hook, python3 }:

mkShell {
  sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];
  sopsCreateGPGHome = true;
  nativeBuildInputs = [
    sops-import-keys-hook
    python3.pkgs.invoke
  ];
}
