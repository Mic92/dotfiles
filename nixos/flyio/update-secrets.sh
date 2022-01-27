#!/usr/bin/env bash

set -x
flyctl secrets set \
    TLS_CERT=$(ssh root@eve.i cat "/var/lib/acme/dns.thalheim.io/cert.pem" | base64 -w0) \
    TLS_KEY=$(ssh root@eve.i cat "/var/lib/acme/dns.thalheim.io/key.pem" | base64 -w0)
