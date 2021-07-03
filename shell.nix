{ drone-cli
, mkShell
, sops-import-keys-hook
}:

mkShell {
  DRONE_SERVER = "https://drone.thalheim.io";
  sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];
  sopsCreateGPGHome = true;
  nativeBuildInputs = [
    sops-import-keys-hook
    drone-cli
  ];
}
