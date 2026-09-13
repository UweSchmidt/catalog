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
syncDir="no"
updateKeywords="no"
updateChecksum="no"
updateAddress="no"
geometries="320x320 1400x1050 1920x1200 3840x2160"
iconGeometries="320x240"

function usage() {
    pname=$(basename $0)
    cat <<EOF
$pname

Usage: $pname
          [-H|--host HOST] [-P|--port PORT]
          [-h | --help]
          [--debug | (-t|--trace) | (-v|--verbose) | (-i|--info) |
          (-w|--warnings) | --errors | (-q|--quiet)]
          (-c|--collection) COL-PATH | (-p|--photos) COL-PATH |
          (-g|--geo) IMG-GEO |
          (-a|--geo-address) | (-k|--keywords) | (-s|sync-fs)

  Prepare a complete catalog collection.
  The collection is given by a relative path pointing into the
  collection hierachy  "$colpx0".
  This includes the following steps:

  .1 filling the image cache for various screen sizes
  .2 optionaly set the geo addresses for entries with GPS metadata
  .3 optionally update the keywords collections
  .4 optionally compute/update checksums for image files

Available options:
  -h, --help              This message
  -H, --host HOST         catalog server host, default: $host0
  -P, --port PORT         catalog server port, default: $port0
  --debug, ..., --quiet   levels of debug information
  -c, --collection        the relative path to the collection to be processed
                          "." or "" for the empty path (whole albums hierachy),
                          default: $colname
  -p, --photos            switch to hierachy of imported photos
                          path prefix is set to "$colphotos0"
  -g, --geo               a list of geometries (<w>x<h>) for which the image cache is build
                          argument "" switches off filling the cache
                          default: all geometries of the screens curently in use
                          ($geometries)
  -s, --sync-fs           if -p is set, synchronise collection with filesystem
                          default: no sync
  -u, --checksum          if -p is set, update checksum of image files
                          default is no update of checksum hashes
  -a, geo-address         if -c is set, update addresses for GPS coordinates
                          default: no update
  -k, --keywords          if -c is set, recompute keyword collections
                          default: no update of keyword collections
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
        -g|--geo)
            geometries="$2"
            iconGeometries=""
            shift
            shift
            ;;
        -a|--geo-address)
            updateAddress="yes"
            shift
            ;;
        -k|--keywords)
            updateKeywords="yes"
            shift
            ;;
        -s|--sync-fs)
            syncDir="yes"
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


function ccl() (
    trc "$@"
    cl=$1
    shift
    $cl "$@"
)

# ----------------------------------------
# check whether server runs

ccl "$client" -q entry "$colpx" > /dev/null
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

hid=$(ccl "$clientl" new-undo "run update-collection.sh for $col1")

# ----------------------------------------
# sync with file system

if [[ "$update" = "PHOTO" && "$syncDir" = "yes" ]]
then
    ccl "$clientl" sync-collection "$col1"
fi

# ----------------------------------------
# set geo addresses

if [[ "$update" = "COL" || "$update" = "PHOTO" ]] && [[ "$updateAddress" = "yes" ]]
then
    ccl "$clientl" geo-address "$col1"
fi

# ----------------------------------------
# update keywords

if [[ "$update" = "COL" && "$updateKeywords" = "yes" ]]
then
    ccl "$clientl" new-keywords
fi

# ----------------------------------------
# update checksums


if [[ "$update" = "PHOTO" && "$updateChecksum" = "yes" ]]
then
    col2=$(echo "$col1" | sed -e 's|/collections||')
    ccl "$clientl" update-checksum "$col2"
fi

# ----------------------------------------
# fill the image cache for the different screen sizes currently in use
#
#  320x320    icons
# 1400x1050   Canon Beamer
# 1920x1200   Macbook Pro
# 3840x2160   4K Monitor

if [[ "$update" = "COL" || "$update" = "PHOTO" ]]
then
    for g in $geometries
    do
        ccl "$clientl" img-cache -i img -g "$g" "$col1"
    done

    for g in $iconGeometries
    do
        ccl "$clientl" img-cache -i icon -g "$g" "$col1"
    done
fi

# ----------------------------------------
