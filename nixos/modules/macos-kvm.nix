{ ... }: {
  imports = [
    ./libvirt.nix
  ];
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1
  '';

  # TODO: get kvmgt working
  #virtualisation.kvmgt.enable = true;
  #virtualisation.kvmgt.vgpus."i915-GVTg_V5_8".uuid = "a297db4a-f4c2-11e6-90f6-d3b88d6c9525";
}
