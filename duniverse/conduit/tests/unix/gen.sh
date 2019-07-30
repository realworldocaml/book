#!/bin/sh
set -e
set -o nounset
cat >server.conf <<EOF
[ req ]
default_bits		= 2048
distinguished_name	= req_distinguished_name
prompt			= no
encrypt_key		= no
x509_extensions		= v3_ca

[ req_distinguished_name ]
CN		       = localhost

[ CA_default ]
copy_extensions = copy

[ alternate_names ]
DNS.2=localhost

[ v3_ca ]
subjectAltName=@alternate_names
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid:always,issuer:always
basicConstraints = critical,CA:true
keyUsage=keyCertSign,cRLSign,digitalSignature,keyEncipherment,nonRepudiation
EOF

openssl req -days 1 -x509 -config server.conf -new -keyout server.key -out server.pem
