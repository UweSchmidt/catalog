#!/bin/bash

# start catalog server
# this script must be started
# from within catalog dir as ./bin/start-catalog-server.sh

die () {
    echo $1 >&2
    echo "exit 1"
    exit 1
}

# develop or release dir?

catroot="$(basename $(dirname $PWD))"
reldir="Bilder"
port="3001"

[[ "$catroot" = "$reldir" ]]         || port="3333"  # develop dir

[[ "$(basename $PWD)" = "catalog" ]] || die "current working dir must be 'catalog' "

[[ -d "data" ]]                      || die "no subdir data"

cd "data"                            # server must be started in subdir data

server="../bin/servant-polysemy"
catdir="catalog"
catalog="photos-0.5.pathid.json"

[[ -d "$catdir" ]]                   || die "no subdir '$catdir'"
[[ -f "$catdir/$catalog" ]]          || die "no catalog '$catdir/$catalog'"
[[ -x "$server" ]]                   || die "no executable '$server'"

$server \
    -P "$port" \
    -a "$catdir/$catalog" \
    -j "${catdir}-journal-$(date "+%Y-%m-%dT%H:%M:%S").json" \
    --save-hash-and-path-ix
