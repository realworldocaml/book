#!/usr/bin/env bash

./echo_pipe.native -uppercase &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
nc 127.0.0.1 8765 < /dev/urandom > /dev/null &
