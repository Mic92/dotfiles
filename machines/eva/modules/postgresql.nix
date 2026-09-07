{ pkgs, ... }:
{
  # keep in sync with eve: lldap-sync pipes eve's pg_dump into this server
  services.postgresql.package = pkgs.postgresql_18;
}
