# eve is the retiolum enrolment host: `tinc -n retiolum invite` here
# registers the node in kartei through the hooks in enroll.py, which open
# PRs as the kartei-enroll GitHub App.
#
#   sudo -u tincr tinc -n retiolum invite -e KARTEI_NS=<namespace> <node>
#   sudo -u tincr tinc -n retiolum invite --replace <node>
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  gen = config.clan.core.vars.generators.kartei-enroll;
  hostData = import (self.inputs.kartei + "/modules/retiolum/hosts.nix") { inherit lib; };
  hostsJson = pkgs.writeText "kartei-hosts.json" (
    builtins.toJSON (lib.mapAttrs (n: o: o // { ns = hostData.hosts.${n}.owner; }) hostData.own)
  );
  hooks =
    pkgs.runCommand "kartei-enroll"
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
        buildInputs = [ pkgs.python3 ];
      }
      ''
        install -Dm755 ${./enroll.py} $out/libexec/enroll.py
        patchShebangs $out/libexec
        for h in invitation-created invitation-accepted; do
          makeWrapper $out/libexec/enroll.py $out/bin/$h --add-flags $h \
            --prefix PATH : ${lib.makeBinPath [ pkgs.openssl ]} \
            --set KARTEI_HOSTS_JSON ${hostsJson} \
            --set KARTEI_GITHUB_APP_ID 4851082 \
            --set KARTEI_GITHUB_INSTALLATION_ID 159529403 \
            --set KARTEI_GITHUB_KEY_FILE ${gen.files.private-key.path}
        done
      '';
in
{
  environment.etc."tinc/retiolum/invitation-created".source = "${hooks}/bin/invitation-created";
  environment.etc."tinc/retiolum/invitation-accepted".source = "${hooks}/bin/invitation-accepted";

  # Populated via `clan vars generate` from the .pem GitHub generates.
  clan.core.vars.generators.kartei-enroll = {
    files.private-key.owner = "tincr";
    prompts.private-key = {
      description = "GitHub App private key (PEM) for krebs/kartei-enroll";
      type = "multiline";
      persist = true;
    };
    script = ''
      cp "$prompts/private-key" "$out/private-key"
    '';
  };
}
