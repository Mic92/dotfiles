{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
}:

rustPlatform.buildRustPackage rec {
  pname = "vaultwarden_ldap";
  version = "2.2.1";

  src = fetchFromGitHub {
    owner = "ViViDboarder";
    repo = "vaultwarden_ldap";
    rev = "v${version}";
    hash = "sha256-+bwlT2+7sZgBfqzfZojxPpsSIeLuBzc0VPpNWk8ql0c=";
  };

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ openssl ];

  cargoHash = "sha256-RtAfx9A9TgusNTLJK6/M2lkj4mEBzNLoQKxjw690FfY=";

  meta = {
    description = "LDAP directory connector for vaultwarden";
    homepage = "https://github.com/ViViDboarder/vaultwarden_ldap";
    license = lib.licenses.gpl3;
    platforms = lib.platforms.all;
    mainProgram = "vaultwarden_ldap";
  };
}
