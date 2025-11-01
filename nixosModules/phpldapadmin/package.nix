{
  lib,
  fetchFromGitHub,
  php84,
  buildNpmPackage,
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

  # Build frontend assets separately
  frontend = buildNpmPackage rec {
    pname = "phpldapadmin-frontend";
    version = "2.3.4";

    src = fetchFromGitHub {
      owner = "leenooks";
      repo = "phpLDAPadmin";
      rev = version;
      hash = "sha256-GwrYzqZ9fA0XY2D/pQvgisI92XQPkYv60A2BzrvDaW0=";
    };

    php = phpWithExtensions;

    vendorHash = "sha256-kmWwXD5coWzu3bF105e6QTNiN6p9i1OGz849jsPeN4Y=";

      runHook postInstall
    '';
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

  postInstall = ''
    # Remove development files
    rm -rf $out/share/php/phpldapadmin/{tests,node_modules,.git*,.env.example}

    # Copy all built frontend assets
    cp -r ${frontend}/* $out/share/php/phpldapadmin/public/

    # Create writable directories structure
    mkdir -p $out/share/php/phpldapadmin/storage/framework/{cache,sessions,views}
    mkdir -p $out/share/php/phpldapadmin/storage/logs
    mkdir -p $out/share/php/phpldapadmin/bootstrap/cache
  '';

    passthru.updateScript = nix-update-script { };
    passthru.frontend = frontend;

    meta = with lib; {
      description = "Web-based LDAP administration interface built on Laravel";
      homepage = "https://github.com/leenooks/phpLDAPadmin";
      license = licenses.gpl2Only;
      maintainers = with maintainers; [ ];
      platforms = platforms.unix;
    };
  }
)
