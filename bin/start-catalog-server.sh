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
loglevel="--info"

[[ "$catroot" = "$reldir" ]]         || port="3333"  # develop dir
[[ "$catroot" = "$reldir" ]]         || loglevel="--verbose"

# options: -P --port, -j --journal, -v --verbose, -t --trace

while [[ $# -gt 0 ]]; do
    case $1 in
        -P|--port)
            port="$2"  # overwrite default port 3001 or 3333
            shift
            shift
            ;;
        -j|--journal)
            journal="$2"
            shift
            shift
            ;;
        -i|--info|-v|--verbose|-t|--trace|-w|--warnings|-q|--quiet|--errors|--debug)
            loglevel="$1"
            shift
            ;;
        -*|--*)
            die "Unknown option: $1"
            ;;
        *)
            die "Illegal argument(s): $*"
            ;;
    esac
done


[[ "$(basename $PWD)" = "catalog" ]] || die "current working dir must be 'catalog' "

[[ -d "data" ]]                      || die "no subdir data"

cd "data"                            # server must be started in subdir data

server="../bin/servant-polysemy"
catdir="catalog"
catalog="photos-0.5.pathid.json"

[[ "$journal" != "" ]]               || journal="${catdir}-journal-$(date "+%Y-%m-%dT%H:%M:%S").json"

[[ -d "$catdir" ]]                   || die "no subdir '$catdir'"
[[ -f "$catdir/$catalog" ]]          || die "no catalog '$catdir/$catalog'"
[[ -x "$server" ]]                   || die "no executable '$server'"

set -x
exec $server \
     $loglevel \
     -P "$port" \
     -a "$catdir/$catalog" \
     -j "$journal" \
     --save-hash-and-path-ix
