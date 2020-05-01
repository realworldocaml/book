#!/bin/sh -e

echo Setting up a /conduit path in xenstore
xenstore-rm /conduit
xenstore-write /conduit ""
xenstore-chmod /conduit b0
