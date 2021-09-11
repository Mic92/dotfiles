{ mkShell, sops-import-keys-hook }:

mkShell {
  sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];
  sopsCreateGPGHome = true;
  nativeBuildInputs = [
    sops-import-keys-hook
  ];
}
