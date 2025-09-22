#!/bin/bash

# start catalog server
# the server is determined by the location of this script

die () {
    echo $1 >&2
    echo "exit 1"
    exit 1
}

function trc() {
    echo '#' "$@" 1>&2
}

# compute physical dir path of this script
res=""

function physDir() {
    local sp="$1"
    local wd=$(pwd -P)
    local ad=

    # abs / rel path ?

    if echo $sp | grep -e '^/'
    then
        ad=$(dirname "$sp")
    else
        ad=$(dirname "$wd/$sp")
    fi

    { pushd "$ad"; res=$(pwd -P); popd; } >/dev/null

    # trc "$wd $sp $ad $res"
    trc physDir: result: "$res"
}

# the supported catalog servers

orgCat="/Volumes/8TB-SieGeht/home/uwe/Bilder/catalog"      # production server on disk 8TB-SieGeht (at host scheibe)
devCat="/Users/uwe/haskell/catalog"                        # development serveer at scheibe or schwarzbuch
extCat="/Volumes/5TB-Diakasten/home/uwe/Bilder/catalog"    # view only server on disk 5TB-Diakasten
testCat="/Users/uwe/tmp/catalog"                           # test server ~/tmp/catalog

physDir "$0"
curCat=$(dirname "$res")

function isOrg() { [[ "$curCat" == "$orgCat" ]]; }
function isDev() { [[ "$curCat" == "$devCat" ]]; }
function isExt() { [[ "$curCat" == "$extCat" ]]; }
function isTest() { [[ "$curCat" == "$testCat" ]]; }

isOrg || isDev || isExt || isTest || die "unknown catalog dir: $curCat"

host=$(hostname | sed -e 's|.local||')
arch=$(arch)

# default options

port="3001"
loglevel="--info"
noCatalogSync=

if isDev
then
    port="3333"
    loglevel="--verbose"
fi

if isExt || isTest
then
    port="3456"
    loglevel="--verbose"
    noCatalogSync="--no-catalog-sync"
fi

# options: -P --port, -j --journal, -v --verbose, -t --trace, --no-sync

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
        --no-catalog-sync)
            noCatalogSync="$1"
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

trc "cd $curCat"
cd "$curCat"                         # cd into catalog dir

[[ -d "data" ]]                      || die "in catalog dir no subdir data found"

cd "data"                            # server must be started in subdir data

server="../bin/$arch/servant-polysemy"
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
     $noCatalogSync \
     --save-hash-and-path-ix
