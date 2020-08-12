{ sops
, drone-cli
, mkShell
, sops-pgp-hook
}:

mkShell {
  DRONE_SERVER = "https://drone.thalheim.io";
  nativeBuildInputs = [
    sops
    drone-cli
    sops-pgp-hook
  ];
  sopsPGPKeyDirs = [
    "./nixos/secrets/keys/hosts"
    "./nixos/secrets/keys/users"
  ];
}
