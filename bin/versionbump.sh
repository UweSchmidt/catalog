#!/bin/bash

# set -x

version="0.5.10.0"
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
bump src/polysemy-tools/polysemy-tools.cabal
bump src/catalog-polysemy/catalog-polysemy.cabal
bump src/client-polysemy/client-polysemy.cabal
bump src/servant-polysemy/servant-polysemy.cabal

# common libs and sources
bump src/catalog-data/catalog-data.cabal
bump src/catalog-data/src/Catalog/Version.hs

# html and javascript sources
bump data/assets/html/edit.html
bump data/assets/html/edit-4.5.0.html
bump data/assets/html/show.html
bump data/assets/javascript/rpc-servant.js

set -x

git status
git commit -a -m "version bump to $version"
git tag "catalog-$version-$dat"
