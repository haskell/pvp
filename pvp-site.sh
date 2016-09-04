#!/bin/bash -e

shopt -s globstar
set -x

cabal new-build exe:pvp-site

dist-newstyle/build/**/pvp-site-1.0/**/build/pvp-site/pvp-site "$@"
