{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    (python3.withPackages (
      ps: with ps; [
        # Testing dependencies
        pytest
      ]
    ))
  ];
}
