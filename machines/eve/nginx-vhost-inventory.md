# `eve.i` nginx vhost inventory

The entries below were extracted from
`/nix/store/3zf38l8w93wm4gif5ac6cy2jz3ffpdaf-nginx.conf` (copied to
`/tmp/eve-nginx.conf` on 2024‑11‑18). Line numbers reference that copy so it is
easy to cross‑check the active nginx configuration on the server.

## Globals worth mirroring in Caddy

- TLS: every server enables HTTP/2, HTTP/3 and reuses certificates from
  `/var/lib/acme/**/` (mostly `thalheim.io`, but `ca.r`, `ldap.thalheim.io`, and
  `mergebot.thalheim.io` use their own issuers). HSTS
  (`Strict-Transport-Security`) is set globally and should either be replicated
  via `header` in the Caddyfile or moved behind a CDN/WAF.
- Compression: both gzip and brotli are enabled with long allowlists. Caddy
  ships with automatic gzip/zstd; if brotli is required double‑check the default
  encoders.
- Proxy defaults: `proxy_http_version 1.1`, `proxy_set_header Connection ""`
  (from `map $http_upgrade`) and the include file
  `/nix/store/vjhg96...-nginx-recommended-proxy_set_header-headers.conf` set the
  canonical `Host`, `X-Real-IP`, `X-Forwarded-*` headers for every `proxy_pass`.
  Caddy’s `reverse_proxy` already sets these, but any custom additions (e.g.
  Authelia specific headers) must be reimplemented manually.
- Limits: default `client_max_body_size 10m` at `http` scope, with per‑site
  overrides (Bitwarden, Paperless, Snappymail, Nextcloud, etc.). Replicate
  per‑site overrides via `request_body` or `header`/`handle` blocks.
- AuthN: The Authelia service lives at `http://127.0.0.1:9091`. Sites that
  require SSO use `auth_request /authelia` + an internal location proxying to
  Authelia’s `/api/verify`. This flow needs to be ported to Caddy’s `handle` +
  `reverse_proxy` (or a dedicated `forward_auth` module).

## Suggested Caddyfile building blocks

```caddyfile
(reverse_proxy_common) {
	@ws {
		header Connection *Upgrade*
		header Upgrade websocket
	}
	reverse_proxy {args.0} {
		header_up X-Real-IP {http.request.remote}
		header_up X-Forwarded-For {http.request.remote}
		header_up X-Forwarded-Proto {http.request.scheme}
		header_up X-Forwarded-Host {http.request.host}
		header_up +Upgrade {http.request.header.Upgrade}
		header_up +Connection {http.request.header.Connection}
	}
}

(authelia_forward_auth) {
	forward_auth http://127.0.0.1:9091 {
		uri /api/verify
		copy_headers Remote-User Remote-Groups Remote-Email Remote-Name
		header_up X-Original-URL {http.request.scheme}://{http.request.host}{http.request.uri}
		header_up X-Forwarded-Proto {http.request.scheme}
		header_up X-Forwarded-Host {http.request.host}
		header_up X-Forwarded-For {http.request.remote}
	}
}

(php_site) {
	root * {args.0}
	encode gzip
	php_fastcgi unix/{args.1}
	file_server
}
```

- `reverse_proxy_common` centralizes header forwarding and WebSocket upgrades;
  use it via `handle { import reverse_proxy_common http://localhost:3002 }`.
- `authelia_forward_auth` mirrors nginx’s `auth_request` flow. Combine it with
  `handle` blocks in each protected site:

  ```caddyfile
  @protected {
  	not path /authelia-health
  }
  handle @protected {
  	import authelia_forward_auth
  	reverse_proxy http://127.0.0.1:5678 {
  		header_up Remote-User {http.auth.user.id}
  	}
  }
  ```

- `php_site` is a thin helper for Snappymail/phpLDAPadmin/FreshRSS. Swap
  `{args.0}` and `{args.1}` per site (e.g.
  `import php_site /var/lib/phpldapadmin/app/public /run/phpfpm/phpldapadmin.sock`).
- For SSE endpoints (n8n `/mcp/`, Buildbot SSE) wrap `reverse_proxy` with a
  custom transport:

  ```caddyfile
  reverse_proxy http://127.0.0.1:5678 {
  	transport http {
  		versions h2c 1.1
  		read_buffer 0
  		response_header_timeout 86400s
  	}
  	flush_interval -1
  }
  ```

These snippets can be placed in `machines/eve/Caddyfile` once the Caddy module
is added to the NixOS configuration.

## Where each vhost is defined in the Nix tree

The table below links every hostname seen in `/tmp/eve-nginx.conf` to the Nix
file that currently defines it (derived via a quick static scan of `machines/`
plus `nixosModules/`). A `—` entry means the host only exists in the rendered
nginx.conf today – it still has to be ported into this repo before we can
reimplement it in Caddy.

| Hostname                    | Module file / line                             |
| --------------------------- | ---------------------------------------------- |
| `atuin.thalheim.io`         | machines/eve/modules/atuin.nix:4               |
| `auth.devkid.net`           | machines/eve/modules/authelia.nix:192          |
| `auth.thalheim.io`          | machines/eve/modules/authelia.nix:177          |
| `bitwarden.thalheim.io`     | machines/eve/modules/vaultwarden.nix:125       |
| `blog.devkid.net`           | machines/eve/modules/nginx/devkid.net.nix:9    |
| `boot.thalheim.io`          | machines/eve/modules/nginx/homepage.nix:3      |
| `buildbot.thalheim.io`      | machines/eve/modules/buildbot.nix:81           |
| `ca.r`                      | machines/eve/modules/step-ca/default.nix:20    |
| `cache.thalheim.io`         | machines/eve/modules/harmonia.nix:10           |
| `cloud.thalheim.io`         | machines/eve/modules/nextcloud.nix:66          |
| `pim.devkid.net`            | —                                              |
| `devkid.net`                | machines/eve/modules/nginx/devkid.net.nix:3    |
| `dl.devkid.net`             | machines/eve/modules/nginx/dl.nix:22           |
| `dl.lekwati.com`            | machines/eve/modules/nginx/dl.nix:24           |
| `dl.thalheim.io`            | machines/eve/modules/nginx/dl.nix:23           |
| `flood.thalheim.io`         | nixosModules/flood.nix:32                      |
| `git.thalheim.io`           | machines/eve/modules/gitea/default.nix:126     |
| `glowing-bear.thalheim.io`  | machines/eve/modules/nginx/glowing-bear.nix:4  |
| `goatcounter.thalheim.io`   | machines/eve/modules/goatcounter.nix:31        |
| `grafana.thalheim.io`       | machines/eve/modules/grafana.nix:62            |
| `ip.thalheim.io`            | machines/eve/modules/nginx/ip.nix:3            |
| `ldap.thalheim.io`          | machines/eve/modules/phpldapadmin.nix:97       |
| `lekwati.com`               | machines/eve/modules/nginx/glowing-bear.nix:14 |
| `localhost`                 | —                                              |
| `127.0.0.1`                 | —                                              |
| `[::1]`                     | —                                              |
| `mail.thalheim.io`          | machines/eve/modules/snappymail.nix:40         |
| `matrix.thalheim.io`        | machines/eve/modules/dendrite.nix:135          |
| `mergebot.thalheim.io`      | —                                              |
| `mta-sts.devkid.net`        | machines/eve/modules/nginx/mta-sts.nix:16      |
| `mta-sts.lekwati.com`       | machines/eve/modules/nginx/mta-sts.nix:17      |
| `mta-sts.thalheim.io`       | machines/eve/modules/nginx/mta-sts.nix:15      |
| `n8n-api.thalheim.io`       | machines/eve/modules/n8n/default.nix:160       |
| `n8n.thalheim.io`           | machines/eve/modules/n8n/default.nix:98        |
| `owncast.thalheim.io`       | machines/eve/modules/owncast.nix:9             |
| `paperless-api.thalheim.io` | machines/eve/modules/paperless.nix:177         |
| `paperless.thalheim.io`     | machines/eve/modules/paperless.nix:129         |
| `phd.thalheim.io`           | —                                              |
| `photoprism.thalheim.io`    | machines/eve/modules/nginx/photoprism.nix:4    |
| `reports.thalheim.io`       | machines/eve/modules/nginx/homepage.nix:40     |
| `retiolum.thalheim.io`      | machines/eve/modules/nginx/retiolum.nix:3      |
| `rspamd.thalheim.io`        | machines/eve/modules/rspamd/rspamd.nix:109     |
| `rss-api.devkid.net`        | machines/eve/modules/freshrss/default.nix:65   |
| `rss.devkid.net`            | machines/eve/modules/freshrss/default.nix:110  |
| `rss.thalheim.io`           | machines/eve/modules/freshrss/default.nix:58   |
| `shiori.thalheim.io`        | machines/eve/modules/shiori/default.nix:24     |
| `syncthing.thalheim.io`     | machines/eve/modules/syncthing.nix:9           |
| `thalheim.io`               | machines/eve/modules/nginx/homepage.nix:9      |
| `upterm.thalheim.io`        | nixosModules/uptermd.nix:13                    |
| `warez.r`                   | nixosModules/rtorrent.nix:26                   |
| `www.devkid.net`            | machines/eve/modules/nginx/devkid.net.nix:15   |
| `www.thalheim.io`           | machines/eve/modules/nginx/homepage.nix:34     |

## Reverse proxies and app frontends

| Domain(s) (line)                                      | Backend / behaviour                                                                                                                                | Notes for migration                                                                                                                                                                                          |
| ----------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `atuin.thalheim.io` (`/tmp/eve-nginx.conf:71`)        | Plain reverse proxy to `http://localhost:8888`.                                                                                                    | No auth; keep websocket upgrade handling.                                                                                                                                                                    |
| `auth.devkid.net`, `auth.thalheim.io` (lines 101/136) | Authelia portal on `http://127.0.0.1:9091`.                                                                                                        | Extra headers (`X-Original-URL`, `X-Forwarded-*`) are required by Authelia; replicate via `reverse_proxy { header_up ... }`.                                                                                 |
| `bitwarden.thalheim.io` (171)                         | `http://localhost:3011` for UI/API, `/notifications/hub` → `http://localhost:3012`, `/notifications/hub/negotiate` → :3011.                        | Requires websocket upgrades and `client_max_body_size 128M`.                                                                                                                                                 |
| `buildbot.thalheim.io` (273)                          | `http://127.0.0.1:8010` for UI/SSE/WebSocket endpoints; `/nix-outputs/` serves files via `alias`.                                                  | Reproduce long proxy timeouts, SSE (`proxy_buffering off`), WS upgrades, and the static alias (Caddy `handle_path /nix-outputs/*` with `root`).                                                              |
| `ca.r` (315)                                          | `https://localhost:1443`; `.well-known/acme-challenge` served from `/var/lib/acme/acme-challenge`; `/ca.crt` aliases the step CA cert.             | Uses dedicated certs from `/var/lib/acme/ca.r`. Need Caddy `tls { cert_file ... key_file ... }` or external issuer; also exposes ACME HTTP-01 path publicly.                                                 |
| `cache.thalheim.io` (356)                             | `http://127.0.0.1:5000`.                                                                                                                           | Manually forces `proxy_redirect http:// https://` and sets Upgrade headers; replicate via `header_up`/`rewrite`.                                                                                             |
| `cloud.thalheim.io`, `pim.devkid.net` (391)           | Nextcloud PHP app rooted at `/nix/store/...nextcloud-32.0.1-with-apps`, PHP served by `unix:/run/phpfpm/nextcloud.sock`.                           | Heavy location logic (DAV rewrites, `/.well-known`, `remote.php`, MIME overrides) plus `client_max_body_size 512M`. Port the official Nextcloud Caddy snippet and keep the custom gzip and header overrides. |
| `flood.thalheim.io` (644)                             | Static SPA assets + `/api` proxy to `http://localhost:3003` guarded by Authelia (`/authelia` internal verify, `error_page 401` redirect).          | Needs Authelia forward-auth replication and SPA fallback (`try_files ... /index.html`).                                                                                                                      |
| `git.thalheim.io` (698)                               | `http://localhost:3002`; exposes `/robots.txt` via `alias`.                                                                                        | Straightforward reverse proxy plus static file.                                                                                                                                                              |
| `glowing-bear.thalheim.io` (730)                      | Static UI from `/nix/store/...glowing-bear-0.9.0`, `/weechat` proxied to `http://127.0.0.1:4242` (WebSocket).                                      | Split static + WS proxy in Caddy (`handle_path /weechat*`).                                                                                                                                                  |
| `goatcounter.thalheim.io` (766)                       | `http://localhost:3004`.                                                                                                                           | Relies on original `Host`/`X-Forwarded-*`; Caddy handles automatically.                                                                                                                                      |
| `grafana.thalheim.io` (799)                           | `http://localhost:3001`.                                                                                                                           | Default reverse proxy.                                                                                                                                                                                       |
| `mail.thalheim.io` (957)                              | Snappymail PHP app with root `/nix/store/...snappymail-2.38.2` served via `unix:/run/phpfpm/snappymail.sock`.                                      | Enforce `client_max_body_size 256M` and protect `/data`. Use `php_fastcgi` with the socket.                                                                                                                  |
| `matrix.thalheim.io` (996)                            | Static Element web root; `/_{dendrite,matrix,synapse}` → `http://127.0.0.1:8043`; select `/_matrix` client endpoints → `http://127.0.0.1:8009`.    | Needs per-path upstreams and `client_max_body_size 30M`.                                                                                                                                                     |
| `mergebot.thalheim.io` (1050)                         | `http://turingmachine.r:8081` with WS upgrades; dedicated certs under `/var/lib/acme/mergebot.thalheim.io`.                                        | Keep ACME challenge exposure + custom cert source.                                                                                                                                                           |
| `n8n-api.thalheim.io` (1175)                          | `http://127.0.0.1:5678`; `/mcp/` path tuned for SSE (no buffering, Accept header, 24h timeouts).                                                   | Important to clear auth headers (`header_up -X-Email` etc.) and reuse SSE-friendly options.                                                                                                                  |
| `n8n.thalheim.io` (1234)                              | Same backend as above but fronted by Authelia. Also exposes webhook paths without auth.                                                            | Recreate Authelia verify endpoint, propagate user/email/group headers, and keep the `@authelia_proxy_signin` redirect logic.                                                                                 |
| `owncast.thalheim.io` (1302)                          | `http://localhost:3012` with WS upgrade.                                                                                                           | Simple reverse proxy.                                                                                                                                                                                        |
| `paperless-api.thalheim.io` (1335)                    | `http://127.0.0.1:28981`; large body/timeouts; clears `Remote-User`.                                                                               | Ensure 200 MB limit and 300 s proxy timeouts.                                                                                                                                                                |
| `paperless.thalheim.io` (1375)                        | Same backend as API but guarded by Authelia and forwards `Remote-User`.                                                                            | Mirror Authelia flow + `error_page 401` redirect to `auth.thalheim.io`.                                                                                                                                      |
| `photoprism.thalheim.io` (1461)                       | `http://blob64.r:2342` with WS support.                                                                                                            | Straightforward.                                                                                                                                                                                             |
| `rspamd.thalheim.io` (1551)                           | `http://localhost:11334`.                                                                                                                          | No auth.                                                                                                                                                                                                     |
| `rss-api.devkid.net` (1580)                           | FreshRSS API served via PHP (`fastcgi_pass unix:/run/phpfpm/freshrss.sock`) with greader rewrites.                                                 | No Authelia; explicitly clears `REMOTE_USER`.                                                                                                                                                                |
| `rss.devkid.net` (1627)                               | Full FreshRSS UI using PHP + Authelia forward-auth.                                                                                                | Needs `auth_request` equivalent, `REMOTE_USER` passthrough, and per-path FastCGI rewrite logic.                                                                                                              |
| `shiori.thalheim.io` (1816)                           | `http://localhost:4378`.                                                                                                                           | Basic reverse proxy.                                                                                                                                                                                         |
| `syncthing.thalheim.io` (1846)                        | `http://localhost:8384/` with 600 s timeouts.                                                                                                      | Mirror long read/send timeouts.                                                                                                                                                                              |
| `thalheim.io` (1881)                                  | Static site `/var/www/higgsboson.tk`, `/http-bind` proxied to `http://localhost:5280/http-bind`, `.well-known/matrix/{client,server}` return JSON. | Use `handle` blocks for JSON responses; keep proxy buffering disabled for BOSH.                                                                                                                              |

## Additional FastCGI/PHP apps

| Domain(s) (line)                            | Backend / behaviour                                                                                   | Notes                                                                                                                                                            |
| ------------------------------------------- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `cloud.thalheim.io`, `pim.devkid.net` (391) | Nextcloud via `unix:/run/phpfpm/nextcloud.sock`.                                                      | Includes DAV rewrites, strict deny rules, MIME tweaks, `client_max_body_size 512M`, and gzip overrides. Port carefully or build a dedicated Caddy `handle` tree. |
| `ldap.thalheim.io` (861)                    | phpLDAPadmin served from `/var/lib/phpldapadmin/app/public` via `unix:/run/phpfpm/phpldapadmin.sock`. | ACME challenge exception, dotfile deny, `client_max_body_size 100M`, custom headers (`X-Frame-Options`, `X-Content-Type-Options`).                               |
| `mail.thalheim.io` (957)                    | Snappymail + PHP-FPM (`unix:/run/phpfpm/snappymail.sock`).                                            | Blocks `/data`, enforces 256 MB uploads.                                                                                                                         |
| `rss-api.devkid.net` (1580)                 | FreshRSS API (`fastcgi_pass unix:/run/phpfpm/freshrss.sock`).                                         | Rewrites to `api/greader.php`, clears `REMOTE_USER`.                                                                                                             |
| `rss.devkid.net` (1627)                     | FreshRSS UI + Authelia.                                                                               | Authelia guard on `/` and PHP paths, passes `REMOTE_USER` to FastCGI, redirects unauthenticated users to `https://auth.devkid.net`.                              |

## Static file hosts

| Domain(s) (line)                                                  | Root / behaviour                                        | Notes                                                                                                                                                                                                                                                                                                      |
| ----------------------------------------------------------------- | ------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `blog.devkid.net`, `devkid.net` (219/506)                         | `/var/www/devkid.net`.                                  | Pure static; HTTP blocks already redirect to HTTPS.                                                                                                                                                                                                                                                        |
| `boot.thalheim.io` (246)                                          | `/var/www/boot.thalheim.io`.                            | Static.                                                                                                                                                                                                                                                                                                    |
| `dl.devkid.net`, `dl.lekwati.com`, `dl.thalheim.io` (533/570/607) | `/var/www/dl.*`.                                        | Tokenised downloads: rewrite `/token/path` → `/path?st=token`, `/files/` guarded by `secure_link` that sources secrets from `/run/secrets/nginx-secure-link`. Caddy lacks native `secure_link`, so plan to reimplement (e.g. via `rewrite` + `transform` plugin or move validation into the upstream app). |
| `flood.thalheim.io` (644)                                         | `/nix/store/...flood.../dist/assets`.                   | Static SPA + protected API (see reverse proxy section).                                                                                                                                                                                                                                                    |
| `reports.thalheim.io` (1494)                                      | `/var/www/reports.thalheim.io` with `index index.html`. | Static.                                                                                                                                                                                                                                                                                                    |
| `retiolum.thalheim.io` (1524)                                     | `/var/www/retiolum.thalheim.io`.                        | Static.                                                                                                                                                                                                                                                                                                    |
| `upterm.thalheim.io` (1925)                                       | `/nix/store/hymlxny6wlhka87hbmiqz0y4hh4ff823-webroot`.  | Static.                                                                                                                                                                                                                                                                                                    |
| `warez.r` (1945)                                                  | `/data/torrent/download` with `fancyindex` enabled.     | Enable directory listing equivalents in Caddy (`file_server browse`).                                                                                                                                                                                                                                      |

## Redirects, special responders, and infrastructure

| Domain(s) (line)                                                                    | Behaviour                                                            | Notes                                                                                                             |
| ----------------------------------------------------------------------------------- | -------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| `ip.thalheim.io` (828)                                                              | Returns `$remote_addr` as plain text.                                | In Caddy: `respond "{http.request.remote.host}\n"` with `Content-Type text/plain`.                                |
| `lekwati.com` (910)                                                                 | TLS vhost with no locations defined.                                 | Likely placeholder while content is served elsewhere; decide whether to mirror with a default `respond` in Caddy. |
| `localhost`, `127.0.0.1`, `[::1]` (927)                                             | `/nginx_status` stub_status protected to loopback.                   | Replace with Caddy admin metrics or keep nginx running side-by-side until Caddy replacement exists.               |
| `mta-sts.devkid.net`, `mta-sts.lekwati.com`, `mta-sts.thalheim.io` (1088/1117/1146) | Serve `/ .well-known/mta-sts.txt` from `/nix/store/.../mta-sts.txt`. | Simple static alias.                                                                                              |
| `rss.thalheim.io` (1700)                                                            | `301 → https://rss.devkid.net$request_uri`.                          | Replicate via `redir`.                                                                                            |
| `www.devkid.net`, `www.thalheim.io` (1973–2089)                                     | Permanent redirects to the bare domain equivalents.                  | Use `redir https://target{uri}`.                                                                                  |

## Next steps

1. **Inject documentation into the repo** – this file serves as the canonical
   inventory for Eve’s HTTP estate so we can track migration progress.
2. **Group vhosts by migration complexity** – start with redirect/static hosts,
   then simple reverse proxies, then Authelia‑protected and PHP/FastCGI
   workloads, leaving Nextcloud + secure_link downloads for last.
3. **Design Caddy building blocks** – shared `import` snippets for:
   - Base `reverse_proxy` (headers, websockets) with optional `handle_path`.
   - Authelia forward-auth (one `handle` that proxies to
     `127.0.0.1:9091/api/verify` and populates request headers).
   - PHP fastcgi (`php_fastcgi unix//run/phpfpm/...`) plus static asset caching
     rules for Nextcloud/FreshRSS/Snappymail.
   - Legacy helpers: `respond` for `.well-known/matrix`, SSE timeouts, BOSH
     proxied route.
4. **Plan deployment** – run Caddy on an alternate port (or side-by-side via
   `SO_REUSEPORT`) behind nginx until parity is reached, then flip DNS or
   systemd service; this allows per-vhost validation using curl against the new
   port before cutover.
