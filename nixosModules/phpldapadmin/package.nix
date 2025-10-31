{
  lib,
  fetchFromGitHub,
  php84,
  nix-update-script,
}:

let
  phpWithExtensions = php84.buildEnv {
    extensions = (
      { enabled, all }:
      enabled
      ++ (with all; [
        ldap
        openssl
        fileinfo
      ])
    );
  };
in
phpWithExtensions.buildComposerProject2 (finalAttrs: {
  pname = "phpldapadmin";
  version = "2.3.4";

  src = fetchFromGitHub {
    owner = "leenooks";
    repo = "phpLDAPadmin";
    rev = finalAttrs.version; # No 'v' prefix
    hash = "sha256-GwrYzqZ9fA0XY2D/pQvgisI92XQPkYv60A2BzrvDaW0=";
  };

    patchFlags = [ "-p1" ];

    postInstall = ''
      # Remove development files
      rm -rf $out/share/php/phpldapadmin/{tests,node_modules,.git*,.env.example}

  # Skip npm build for now - Laravel Mix assets can be built at runtime if needed
  # or we can add a separate buildNpmPackage derivation later

  postInstall = ''
    # Remove development files
    rm -rf $out/share/php/phpldapadmin/{tests,node_modules,.git*,.env.example}

    # Create writable directories structure
    mkdir -p $out/share/php/phpldapadmin/storage/framework/{cache,sessions,views}
    mkdir -p $out/share/php/phpldapadmin/storage/logs
    mkdir -p $out/share/php/phpldapadmin/bootstrap/cache
  '';

  passthru.updateScript = nix-update-script { };

    meta = with lib; {
      description = "Web-based LDAP administration interface built on Laravel";
      homepage = "https://github.com/leenooks/phpLDAPadmin";
      license = licenses.gpl2Only;
      maintainers = with maintainers; [ ];
      platforms = platforms.unix;
    };
  }
)
