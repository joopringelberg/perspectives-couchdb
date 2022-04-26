#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd .spago

##### AVAR-MONADASK #####
cd avar-monadask/

rm -Rf AVAR_MONADASK

ln -s ../../../purescript-avar-monadask AVAR_MONADASK

cd ..


##### AFFJAX #####
cd affjax

rm -Rf AFFJAX

ln -s ../../../purescript-affjax AFFJAX

cd ..

