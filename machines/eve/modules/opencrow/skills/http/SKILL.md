---
name: http
description: Make HTTP requests using hurl. Use for accessing websites/apis
---

# hurl

```bash
# GET HTML
hurl <<'EOF'
GET https://example.org
HTTP 200
[Asserts]
xpath "normalize-space(//head/title)" == "Hello world!"
EOF

# Chain requests with captures
hurl <<'EOF'
POST https://api.example.com/login
Content-Type: application/json
{"user": "me", "pass": "secret"}
HTTP 200
[Captures]
token: jsonpath "$.token"

GET https://api.example.com/resource
Authorization: Bearer {{token}}
HTTP 200
EOF
```

Flags: `--variable key=val`, `--test` (assert mode).
