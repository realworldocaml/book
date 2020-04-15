#!/bin/sh

port=4455
polarssl="/opt/bin/mbedtls_ssl_client2 auth_mode=none server_port="

extra_args=""
statfile="/tmp/test_server.status"

testit () {
    /bin/sh -c "cd .. && ./_build/default/lwt/examples/test_server.exe $port > /dev/null && echo foo > $statfile" &

    sleep 0.3

    $polarssl$port $extra_args 2> /dev/null > /dev/null

    sleep 0.3

    if [ -e $statfile ]; then
        result=$(cat $statfile)
        if [ $result = "foo" ]; then
            echo "success with $extra_args"
        else
            echo "failure with $polarssl$port $extra_args (statfile there)"
            exit 1
        fi
        rm $statfile
    else
        echo "failure with $polarssl$port $extra_args"
        exit 1
    fi
    sleep 0.3
    port=$(expr $port + 1)
}

testit

extra_args="force_version=tls1"
testit

extra_args="force_version=tls1_1"
testit

extra_args="force_version=tls1_2"
testit

ciphers="
TLS-DHE-RSA-WITH-AES-256-CBC-SHA
TLS-DHE-RSA-WITH-AES-128-CBC-SHA
TLS-DHE-RSA-WITH-3DES-EDE-CBC-SHA
TLS-RSA-WITH-AES-256-CBC-SHA
TLS-RSA-WITH-AES-128-CBC-SHA
TLS-RSA-WITH-3DES-EDE-CBC-SHA
TLS-RSA-WITH-RC4-128-SHA
TLS-RSA-WITH-RC4-128-MD5"

for i in $ciphers; do
    extra_args="force_ciphersuite=$i"
    testit

    extra_args="force_version=tls1 force_ciphersuite=$i"
    testit

    extra_args="force_version=tls1_1 force_ciphersuite=$i"
    testit

    extra_args="force_version=tls1_2 force_ciphersuite=$i"
    testit
done

tls12_ciphers="
TLS-DHE-RSA-WITH-AES-256-CCM
TLS-DHE-RSA-WITH-AES-128-CCM
TLS-DHE-RSA-WITH-AES-256-CBC-SHA256
TLS-DHE-RSA-WITH-AES-128-CBC-SHA256
TLS-DHE-RSA-WITH-AES-256-GCM-SHA384
TLS-DHE-RSA-WITH-AES-128-GCM-SHA256
TLS-RSA-WITH-AES-256-CCM
TLS-RSA-WITH-AES-128-CCM
TLS-RSA-WITH-AES-256-CBC-SHA256
TLS-RSA-WITH-AES-128-CBC-SHA256
TLS-RSA-WITH-AES-256-GCM-SHA384
TLS-RSA-WITH-AES-128-GCM-SHA256"
for i in $tls12_ciphers; do
    extra_args="force_ciphersuite=$i"
    testit

    extra_args="force_version=tls1_2 force_ciphersuite=$i"
    testit
done
