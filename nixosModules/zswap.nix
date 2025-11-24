{
  # Enable zswap with lz4 compression
  # zswap is a lightweight compressed cache for swap pages
  # that can significantly improve performance on systems with limited RAM

  boot.kernelParams = [
    # Enable zswap
    "zswap.enabled=1"
  ];

  # Load lz4 compression modules
  boot.kernelModules = [
    "lz4"
    "lz4_compress"
  ];

  # Configure zswap with lz4 compressor and z3fold allocator
  boot.extraModprobeConfig = ''
    options zswap enabled=1 compressor=lz4 zpool=z3fold
  '';
}
