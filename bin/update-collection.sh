#!/bin/bash

# start catalog server
# the server is determined by the location of this script

# set -x
set -u

function die () {
    echo $1 >&2
    echo "exit 1"
    exit 1
}

function trc() {
    echo '#' "$@" 1>&2
}

host0="localhost"
# host0="scheibe"
port0=3333
# port0=3001
loglevel="--info"
colname0="2026"

colpx0="/archive/collections/albums"
colphotos0="/archive/collections/photos"
dirphotos0="/archive/photos"

host=$host0
port=$port0
colname="$colname0"
colpx="$colpx0"
update="COL"

function usage() {
    pname=$(basename $0)
    cat <<EOF
$pname

Usage: $pname [-H|--host HOST] [-P|--port PORT] [-h | --help]
          [--debug | (-t|--trace) | (-v|--verbose) | (-i|--info) |
          (-w|--warnings) | --errors | (-q|--quiet)]
          -c COL-PATH | -p COL-PATH

  Prepare a complete catalog collection.
  The collection is given by a relative path pointing into the
  collection hierachy  "$colpx0".
  This includes the following steps:

  .1 filling the image cache for various screen sizes
  .2 set the geo addresses for entries with GPS metadata
  .3 update the keywords collections

Available options:
  -h, --help              This message
  -H, --host HOST         catalog server host, default: $host0
  -P, --port PORT         catalog server port, default: $port0
  --debug, ..., --quiet   levels of debug information
  -c, --collection        the relative path to the collection to be processed
                          "." or "" for the empty path (whole albums hierachy),
                          default: $colname
  -p, --photos            switch to hierachy of imported photos
                          path prefix is set to "$colphoto0"
EOF
}

client=$(hostname | sed -e 's|.local||')
arch=$(arch)

devCat="/Users/uwe/haskell/catalog"

exe="$devCat/bin/$arch/client-polysemy"


while [[ $# -gt 0 ]]; do
    case $1 in
        -P|--port)
            port="$2"  # overwrite default port 3001 or 3333
            shift
            shift
            ;;
        -H|--host)
            host="$2"
            shift
            shift
            ;;
        -i|--info|-v|--verbose|-t|--trace|-w|--warnings|-q|--quiet|--errors|--debug)
            loglevel="$1"
            shift
            ;;
        -c|--collection)
            colname="$2"
            shift
            shift
            ;;
        -p|--photos)
            colname="$2"
            colpx="$colphotos0"
            update="PHOTO"
            shift
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        -*|--*)
            die "Unknown option: $1"
            ;;
        *)
            die "Illegal argument(s): $*"
            ;;
    esac
done

client="$exe -P $port -H $host"
clientl="$client $loglevel"

# ----------------------------------------
# check whether server runs
$client -q entry "$colpx" > /dev/null
[[ $? -eq 0 ]] || die "catalog server \"$client\" not running"


# ----------------------------------------
# check whether collection exists

if [[ "$colname" = "." || "$colname" = "" ]]
then
    col0="$colpx"
else
    col0="$colpx/$colname"
fi

col1=$($client -q entry "$col0" | grep '^/' | head -1 2> /dev/null)

[[ "$col1" != "" ]]  || die "collection/img-dir \""$col0"\" does not exist"

# ----------------------------------------
# create new undo entry

$clientl new-undo "run update-collection.sh for $col1"

# ----------------------------------------
# sync with file system

if [[ "$update" = "PHOTO" ]]
then
    $clientl sync-collection "$col1"
fi

# ----------------------------------------
# set geo addresses

if [[ "$update" = "COL" || "$update" = "PHOTO" ]]
then
    $clientl geo-address "$col1"
fi

# ----------------------------------------
# update keywords

if [[ "$update" = "COL" ]]
then
    $clientl new-keywords
fi

# ----------------------------------------
# update checksums


if [[ "$update" = "PHOTO" ]]
then
    col2=$(echo "$col1" | sed -e 's|/collections||')
    $clientl update-checksum "$col2"
fi

# ----------------------------------------
# fill the image cache for the different screens currently in use

if [[ "$update" = "COL" ]]
then
    for g in 320x320 1400x1050 1920x1200 2560x1440
    do
        $clientl img-cache -i img -g $g "$col1"
    done

    for g in 320x240
    do
        $clientl img-cache -i icon -g $g "$col1"
    done
fi

# ----------------------------------------
