{
  buildPythonApplication,
  lib,
  runCommand,
  setuptools,
}:
buildPythonApplication {
  pname = "nix-uci";
  version = "0.0.1";
  pyproject = true;

  src = runCommand "src" { } ''
    mkdir $out
    cp -r ${../nix_uci} $out/nix_uci
    install ${../pyproject.toml} $out/pyproject.toml
  '';

  build-system = [ setuptools ];

  meta = with lib; {
    description = "Write openwrt's UCI configuration using nixos modules";
    homepage = "https://github.com/Mic92/nix-uci";
    license = licenses.mit;
    maintainers = with maintainers; [ mic92 ];
    platforms = platforms.unix;
  };
}
