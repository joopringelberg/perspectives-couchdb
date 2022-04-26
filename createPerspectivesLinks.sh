#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd .spago

##### AVAR-MONADASK #####
cd avar-monadask/

rm -Rf v2.1.1

ln -s ../../../purescript-avar-monadask v2.1.1

cd ..


##### v12.0.0-with-xhr-cookies #####
cd affjax

rm -Rf v12.0.0-with-xhr-cookies

ln -s ../../../purescript-affjax v12.0.0-with-xhr-cookies

cd ..

