with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    bashInteractive
    cmake
    numactl
    libaio
    libuuid
    openssl
    cunit
  ] ++ rocksdb.buildInputs;

  NIX_CFLAGS_COMPILE = ["-msse4.1"];
  nativeBuildInputs = rocksdb.nativeBuildInputs;
}
