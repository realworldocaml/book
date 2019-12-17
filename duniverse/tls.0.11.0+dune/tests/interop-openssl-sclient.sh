#!/bin/sh

port=4455
s_client_args="s_client -quiet -connect 127.0.0.1:"

extra_args=""
statfile="/tmp/test_server.status"

testit () {
    /bin/sh -c "cd ../ && ./_build/lwt/examples/test_server.native $port > /dev/null && echo foo > $statfile" &

    sleep 0.3

    echo "GET /" | openssl $s_client_args$port $extra_args 2> /dev/null > /dev/null

    sleep 0.3

    if [ -e $statfile ]; then
        result=$(cat $statfile)
        if [ $result = "foo" ]; then
            echo "success with $extra_args"
        else
            echo "failure with openssl $s_client_args $extra_args (statfile there)"
            exit 1
        fi
        rm $statfile
    else
        echo "failure with openssl $s_client_args$port $extra_args"
        exit 1
    fi
    sleep 0.3
    port=$(expr $port + 1)
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
#    if [ $i != "RC4-MD5" ]; then
#        extra_args="-cipher $i"
#        testit
#    else
#        echo "not testing RC4-MD5 without tls version (openssl will use SSLv2 which fails)"
#    fi

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
