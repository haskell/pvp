#!/bin/bash -e

shopt -s globstar

case $(cabal --numeric-version) in
    1.*)
        echo "ERROR: Sorry, you need at least cabal 2.0"
        exit 1
        ;;

    2.[01].*)
        echo "INFO: Detected cabal 2.0; using 'new-run' emulation - if this fails, try removing dist-newstyle/ or upgrade to cabal 2.2 or later"

        cabal new-build exe:pvp-site

        set -x
        exec dist-newstyle/build/**/pvp-site-1.0/**/build/pvp-site/pvp-site "$@"
        ;;

    2.[23].*)
        echo "INFO: Detected cabal 2.2"
        exec cabal new-run exe:pvp-site -- "$@"
        ;;

    2.[45].*|3.*)
        echo "INFO: Detected cabal 2.4+"
        exec cabal v2-run exe:pvp-site -- "$@"
        ;;

    *)  echo "ERROR: Unknown version of cabal detected; you need at least cabal 2.0"
        exit 1
        ;;

esac
