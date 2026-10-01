#!/bin/sh
# A certificate for the tests of wss: ECDSA P-256 for localhost and
# 127.0.0.1, in the certificate directory of the test server (its home:
# TEXMACS_HOME_PATH/server). The self-signed certificate TeXmacs generates
# (generate-self-signed-certificate, which server.scm uses) is Ed25519,
# which the browsers do not accept for TLS; a real one (Let's Encrypt) is
# ECDSA or RSA. The browser accepts this one with browser-run.mjs --insecure.
#
#   sh misc/wasm/remote/wss-cert.sh <TEXMACS_HOME_PATH of the server>
set -e
dir="$1/server"
openssl=$(command -v /opt/homebrew/opt/openssl@3/bin/openssl || command -v openssl)
mkdir -p "$dir"
"$openssl" req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
  -keyout "$dir/key.pem" -out "$dir/cert.pem" -days 30 -subj /CN=localhost \
  -addext "subjectAltName=DNS:localhost,IP:127.0.0.1" 2> /dev/null
echo "certificate: $dir/cert.pem"
