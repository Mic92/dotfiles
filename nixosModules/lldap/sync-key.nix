# Shared ssh keypair for the lldap database replication. Must be defined
# identically on primary and replica (clan requires shared generators to
# match), so both deploy the private key owned by postgres even though
# only the replica uses it.
{ pkgs, ... }:
{
  clan.core.vars.generators.lldap-sync = {
    share = true;
    files.ssh-private-key = {
      secret = true;
      owner = "postgres";
    };
    files.ssh-public-key.secret = false;
    runtimeInputs = [ pkgs.openssh ];
    script = ''
      ssh-keygen -t ed25519 -N "" -f "$out/ssh-private-key" -C "lldap-sync"
      mv "$out/ssh-private-key.pub" "$out/ssh-public-key"
    '';
  };
}
