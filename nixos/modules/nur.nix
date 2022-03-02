{inputs, ...}: {
  nixpkgs.overlays = [inputs.nur.overlay];
}
