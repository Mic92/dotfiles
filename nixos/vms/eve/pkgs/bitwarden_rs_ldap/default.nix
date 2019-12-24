{ stdenv, rustPlatform, fetchFromGitHub, fetchpatch
, pkgconfig, openssl, darwin }:

let
  inherit (darwin.apple_sdk.frameworks) Security CoreServices;
in rustPlatform.buildRustPackage rec {
  pname = "bitwarden_rs_ldap";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "ViViDboarder";
    repo = "bitwarden_rs_ldap";
    rev = "v${version}";
    sha256 = "0zfvy509g7ma8dhra2gz3vk73j5srk4c900ikn715qi9114bk5hz";
  };

  cargoPatches = [
    (fetchpatch ({
      url = "https://github.com/ViViDboarder/bitwarden_rs_ldap/commit/76e803a472c0023b7a759dc5548b6f45a984c8f6.patch";
      sha256 = "040hxd8ic4snr8l96q1xxpmahicdb3xb93cxahr5kj6scj2nzlqg";
    }))
  ];

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ openssl ] ++ stdenv.lib.optionals stdenv.isDarwin [ Security CoreServices ];

  cargoSha256 = "1ciaq6743q1vv1r52v9sp0x627jgycxw4qsxgbq8pbzhss6f71dp";

  meta = with stdenv.lib; {
    description = "LDAP directory connector for bitwarden_rs";
    homepage = https://github.com/ViViDboarder/bitwarden_rs_ldap;
    license = licenses.gpl3;
    maintainers = with maintainers; [ mic92 ];
    platforms = platforms.all;
  };
}
