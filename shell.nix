{ sops
, drone-cli
, mkShell
}:

mkShell {
  DRONE_SERVER = "https://drone.thalheim.io";
  nativeBuildInputs = [
    sops
    drone-cli
  ];
}
