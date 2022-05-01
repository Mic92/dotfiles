{ buildPythonApplication, sops, lib }:
buildPythonApplication {
  pname = "nix-uci";
  version = "0.0.1";
  src = ../.;

  meta = with lib; {
    description = "Write openwrt's UCI configuration using nixos modules";
    homepage = "https://github.com/Mic92/nix-uci";
    license = licenses.mit;
    maintainers = with maintainers; [ mic92 ];
    platforms = platforms.unix;
  };
}
