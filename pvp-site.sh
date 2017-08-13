#!/bin/bash -e

shopt -s globstar

case $(cabal --numeric-version) in
    2.0.*)
        echo "INFO: Detected cabal 2.0; using 'new-run' emulation - if this fails, try removing dist-newstyle/ or upgrade to cabal 2.1+"

        cabal new-build exe:pvp-site

        set -x
        exec dist-newstyle/build/**/pvp-site-1.0/**/build/pvp-site/pvp-site "$@"
        ;;

    2.*)
        echo "INFO: Detected cabal 2.1+"
        exec cabal new-run exe:pvp-site -- "$@"
        ;;
    *)  echo "ERROR: Sorry, you need at least cabal 2.0"
        exit 1
        ;;

esac
