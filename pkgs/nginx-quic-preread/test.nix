# NixOS VM integration test for the ngx_stream_quic_preread module.
#
# Builds nginx with the module statically linked, configures a UDP stream server
# that routes by $quic_preread_server_name, then fires a *real* QUIC Initial
# packet (generated independently by aioquic, with a chosen SNI) at it and
# asserts the datagram is proxied to the SNI-matched backend and not the other.
{
  pkgs,
  lib ? pkgs.lib,
}:
let
  # An independent QUIC client (aioquic) that emits exactly the first Initial
  # datagram for a given SNI — no handshake completion needed.
  sendPy = pkgs.writeText "quic-send.py" ''
    import socket, sys
    from aioquic.quic.configuration import QuicConfiguration
    from aioquic.quic.connection import QuicConnection

    sni, port = sys.argv[1], int(sys.argv[2])
    cfg = QuicConfiguration(is_client=True, server_name=sni, alpn_protocols=["h3"])
    conn = QuicConnection(configuration=cfg)
    conn.connect(("127.0.0.1", port), now=0.0)
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    n = 0
    for data, _addr in conn.datagrams_to_send(now=0.0):
        sock.sendto(data, ("127.0.0.1", port))
        n += 1
    print(f"sent {n} datagram(s) with SNI {sni}")
  '';

  # A one-shot UDP sink: records the first datagram it receives to a file.
  recvPy = pkgs.writeText "quic-recv.py" ''
    import socket, sys
    port, out = int(sys.argv[1]), sys.argv[2]
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.bind(("127.0.0.1", port))
    data, _ = s.recvfrom(65535)
    with open(out, "wb") as f:
        f.write(data)
  '';
in
pkgs.testers.runNixOSTest {
  name = "nginx-quic-preread";

  nodes.machine =
    { pkgs, ... }:
    {
      imports = [ ../../nixosModules/nginx-quic-preread.nix ];

      services.nginx = {
        enable = true;
        # Same base as pkgs.nginx-quic-preread, so additionalModules resolves to
        # the identical nginx+module derivation (shared build).
        package = pkgs.nginxQuic;
        quicPreread.enable = true;
        streamConfig = ''
          map $quic_preread_server_name $backend {
              routed.example  127.0.0.1:5001;
              default         127.0.0.1:5002;
          }

          server {
              listen 127.0.0.1:5443 udp;
              quic_preread on;
              proxy_pass $backend;
              proxy_responses 0;
          }
        '';
      };

      environment.systemPackages = [
        (pkgs.python3.withPackages (ps: [ ps.aioquic ]))
      ];
    };

  testScript = ''
    machine.wait_for_unit("nginx.service")

    def receiver(port, path):
        machine.succeed(f"rm -f {path}")
        machine.succeed(
            f"systemd-run --collect --unit=recv-{port} "
            f"python3 ${recvPy} {port} {path}"
        )

    def stop_receivers():
        machine.succeed(
            "systemctl stop recv-5001.service recv-5002.service 2>/dev/null || true"
        )

    with subtest("SNI matching the map routes to the matched backend"):
        receiver(5001, "/tmp/routed")
        receiver(5002, "/tmp/default")
        machine.sleep(1)
        machine.succeed("python3 ${sendPy} routed.example 5443")
        machine.wait_for_file("/tmp/routed", 10)
        machine.fail("test -e /tmp/default")
        stop_receivers()

    with subtest("Unknown SNI falls through to the default backend"):
        receiver(5001, "/tmp/routed2")
        receiver(5002, "/tmp/default2")
        machine.sleep(1)
        machine.succeed("python3 ${sendPy} other.example 5443")
        machine.wait_for_file("/tmp/default2", 10)
        machine.fail("test -e /tmp/routed2")
        stop_receivers()
  '';

  meta.maintainers = [ ];
}
