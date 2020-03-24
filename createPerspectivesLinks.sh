#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

cd .psc-package/pv0.3.0/aff-sockets

rm -Rf v2.2.0

ln -s ../../../../purescript-aff-sockets v2.2.0

cd ../../..
