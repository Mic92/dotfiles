{
  stdenv,
  lib,
  libbpf,
  bpftools,
  clang,
  llvm,
  elfutils,
  zlib,
}:
stdenv.mkDerivation {
  pname = "hide-nix-store";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    clang
    llvm
    bpftools
  ];
  buildInputs = [
    libbpf
    elfutils
    zlib
  ];

  hardeningDisable = [ "all" ];

  buildPhase = ''
    runHook preBuild

    # vmlinux.h is a minimal hand-written CO-RE header shipped in src/.
    clang -g -O2 -target bpf -D__TARGET_ARCH_x86 \
      -I. -c hide_nix_store.bpf.c -o hide_nix_store.bpf.o

    bpftool gen skeleton hide_nix_store.bpf.o > hide_nix_store.skel.h

    $CC -O2 -g -I. loader.c -lbpf -lelf -lz -o hide-nix-store

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 hide-nix-store $out/bin/hide-nix-store
    runHook postInstall
  '';

  meta = with lib; {
    description = "eBPF LSM that hides /nix/store from selected uids";
    license = licenses.mit;
    platforms = platforms.linux;
    mainProgram = "hide-nix-store";
  };
}
