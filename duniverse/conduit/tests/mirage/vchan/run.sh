#!/bin/sh

sudo xl destroy vchan_server || true
sudo xl destroy vchan_client || true
sudo ./init-xenstore.sh
./build.sh
