#!/bin/sh

s_server_args="s_server -quiet -key ../certificates/server.key -cert ../certificates/server.pem -www -dhparam dh.pem "

pidfile='/tmp/openssl.pid'

extra_args=""

testit () {
    /bin/sh -c "echo \$\$ > $pidfile && exec openssl $s_server_args $extra_args" &

    sleep 0.3

    ../_build/lwt/examples/test_client.native > /dev/null

    if [ $? -eq 0 ]; then
        echo "success with $extra_args"
    else
        echo "failure with openssl $s_server_args $extra_args"
        exit 1
    fi
    cat $pidfile | xargs kill
    rm $pidfile
    sleep 0.5
}

testit

extra_args="-tls1"
testit

extra_args="-tls1_1"
testit

extra_args="-tls1_2"
testit

ciphers="DHE-RSA-AES256-SHA AES256-SHA DHE-RSA-AES128-SHA AES128-SHA"
#OpenSSL <1.1.1:
#EDH-RSA-DES-CBC3-SHA DES-CBC3-SHA RC4-SHA RC4-MD5
for i in $ciphers; do
    extra_args="-cipher $i"
    testit

    extra_args="-tls1 -cipher $i"
    testit

    extra_args="-tls1_1 -cipher $i"
    testit

    extra_args="-tls1_2 -cipher $i"
    testit
done

tls12_ciphers="DHE-RSA-AES256-SHA256 AES256-SHA256 DHE-RSA-AES128-SHA256 AES128-SHA256 AES128-GCM-SHA256 DHE-RSA-AES128-GCM-SHA256 AES256-GCM-SHA384 DHE-RSA-AES256-GCM-SHA384"
for i in $tls12_ciphers; do
    extra_args="-cipher $i"
    testit

    extra_args="-tls1_2 -cipher $i"
    testit
done
