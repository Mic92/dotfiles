{ php }:

php.buildComposerProject2 {
  pname = "kimai-ldap-deps";
  version = "0.1.0";

  src = ./.;

  composerStrictValidation = false;

  php = php.buildEnv {
    extensions = ({ enabled, all }: enabled ++ [ all.ldap ]);
  };

  vendorHash = "sha256-X2g+ASQPg4uvjSNcIqmleMO6oQtaJL6O4eDV14hBdpI=";
}
