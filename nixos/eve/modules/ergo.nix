{ pkgs, ... }:

{
  services.ergochat = {
    enable = true;
    settings.server = {
      name = "ergo.thaheim.io";
      motd = pkgs.writeText "ergo.motd" "Welcome back!";
      password = "$2a$04$mcTzz9./.PT8.npYlXSTkOnsuVKaMvOTEkShQqhFOA27ig8Wuzwxm";

      opers = {
        # default operator named 'admin'; log in with /OPER admin <password>
        admin = {
          # which capabilities this oper has access to
          class = "server-admin";

          # traditionally, operator status is visible to unprivileged users in
          # WHO and WHOIS responses. this can be disabled with 'hidden'.
          hidden = true;

          # custom whois line (if `hidden` is enabled, visible only to other operators)
          whois-line = "is the server administrator";

          # operators can be authenticated either by password (with the /OPER command),
          # or by certificate fingerprint, or both. if a password hash is set, then a
          # password is required to oper up (e.g., /OPER dan mypassword). to generate
          # the hash, use `ergo genpasswd`.
          password = "$2a$04$ltQIKNwu8aR1jImueQTkUuEA4RBRD.zSP3xh5e6gbvvggV7piDHii";
        };
      };
    };
  };
}
