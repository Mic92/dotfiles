{
  lib,
  fetchgit,
  php,
}:

php.buildComposerProject2 (finalAttrs: {
  pname = "kimai-cli";
  version = "2.0.0";

  src = fetchgit {
    url = "https://github.com/kimai/cli";
    tag = finalAttrs.version;
    hash = "sha256-LfKXHN+u8xSoHxyrSPw3QpYoCrApqhu5YqVu+/HfuvY=";
  };

  vendorHash = "sha256-c1e0WgEGhM0apvjw07uYqeLontTdZvUC3G1M+06/ot4=";

  # Replace box phar placeholders that would normally be filled during
  # phar compilation with actual values from the source checkout.
  postPatch = ''
    substituteInPlace src/Constants.php \
      --replace-fail '@git-commit-short@' '${finalAttrs.version}' \
      --replace-fail '@release-date@' '1970-01-01'
  '';

  postInstall = ''
    mkdir -p "$out"/bin
    ln -s "$out"/share/php/kimai-cli/bin/kimai "$out"/bin/kimai
  '';

  meta = {
    description = "Kimai - console application to manage your time-tracking data remotely";
    homepage = "https://github.com/kimai/cli";
    changelog = "https://github.com/kimai/cli/releases/tag/${finalAttrs.version}";
    license = lib.licenses.mit;
    mainProgram = "kimai";
    maintainers = [ ];
  };
})
