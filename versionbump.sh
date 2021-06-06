#/bin/bash

# set -x

version="0.5.7.7"
dat=$(date "+%Y-%m-%d")

LANG="en_US.UTF-8"
LC_ALL="en_US.UTF-8"

bump () {
    f="$1"
    if [[ -f "$f" ]]
    then
        # set the version, separator is word start and word end
        perl -p -i~ \
             -e 's/\b[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+\b/'"$version"'/' \
             "$f"
        # set the version, in quoted string format "0.0.0.0"
        perl -p -i~ \
             -e 's/"[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+"/"'"$version"'"/' \
             "$f"
        # set the date, separator is word start, format 2000-01-01
        perl -p -i~ \
             -e 's/\b[0-9]{4}-[0-9]{2}-[0-9]{2}\b/'"$dat"'/' \
             "$f"
        # set the date, in quoted string format "2000-01-01"
        perl -p -i~ \
             -e 's/"[0-9]{4}-[0-9]{2}-[0-9]{2}"/"'"$dat"'"/' \
             "$f"
    fi
}


# polysemy based libs/execs
bump polysemy-tools/polysemy-tools.cabal
bump catalog-polysemy/catalog-polysemy.cabal
bump client-polysemy/client-polysemy.cabal
bump servant-polysemy/servant-polysemy.cabal

# common libs and sources
bump catalog-data/catalog-data.cabal
bump catalog-data/src/Catalog/Version.hs
bump data/assets/html/edit.html
bump data/assets/html/edit-4.5.0.html
bump data/assets/javascript/rpc-servant.js

set -x

git status
git commit -a -m "version bump to $version"
git tag "catalog-$version-$dat"

stack build
