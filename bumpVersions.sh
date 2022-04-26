#!/usr/bin/env bash

# Modify the version numbers of dependencies as needed. Then run ./bumpVersions.sh to create updated versions of
# * packages.dhall
# * createPerspectivesLinks.sh
# * package.json

AVAR_MONADASK=v2.1.1
AFFJAX=v12.0.0-with-xhr-cookies

sed "s/AVAR_MONADASK/${AVAR_MONADASK}/g;\
s/AFFJAX/${AFFJAX}/g;\
" packages.template.dhall > packages.dhall

sed "s/AVAR_MONADASK/${AVAR_MONADASK}/g;\
s/AFFJAX/${AFFJAX}/g;\
" createPerspectivesLinks.template.sh > createPerspectivesLinks.sh
