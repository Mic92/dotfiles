let
  joerg = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE";
  turingmachine = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBIvs+qolz8zktc/rXU4a7fUNdD+FIJRJU/9cg0YOkLp";
in
{
  "secret1.age".publicKeys = [ joerg turingmachine ];
}
