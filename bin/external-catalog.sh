#!/bin/bash

# set -x

# no uninitialized variables

set -u

# rsync all media files from "Diakaesten" with a media backup

function die() {
    echo $1 1>&2
    exit 1
}

function trc() {
    echo '#' "$@" 1>&2
}

xOpt=
tOpt=
volumeOpt="5TB-Diakasten"

while [[ $# -gt 0 ]]; do
    case "$1" in
        -x)
            xOpt="-x"
            shift
            ;;
        -t|--test)
            tOpt="-t"
            shift
            ;;
        --volume)
            shift
            volumeOpt="$1"
            shift
            ;;
        *)
            die "Illegal option(s) / argument(s): $* "
            ;;
    esac
done

# compute physical dir path of this script
res=""

function physDir() {
    local sp="$1"
    local wd=$(pwd -P)
    local ad=$(dirname "$wd/$sp")

    { pushd "$ad"; res=$(pwd -P); popd; } >/dev/null

    # trc "$wd $sp $ad $res"
    trc physDir: result: "$res"
}

orgCat="/Volumes/8TB-SieGeht/home/uwe/Bilder/catalog"
devCat="/Users/uwe/haskell/catalog"
extCat="/Volumes/$volumeOpt/home/uwe/Bilder/catalog"

physDir "$0"
curCat=$(dirname "$res")

function isTest() { [[ "$tOpt"  = "-t" ]]; }
function isExec() { [[ "$xOpt"  = "-x" ]]; }

function isOrg() { [[ "$curCat" = "$orgCat" ]] || isTest; }
function isDev() { [[ "$curCat" = "$devCat" ]] || isTest; }
function isExt() { [[ "$curCat" = "$extCat" ]] || isTest; }

isOrg || isDev || isExt || die "unknown catalog dir: $curCat"

if isTest
then
    catSrc="$HOME/haskell/catalog"
    catDst="$HOME/tmp/catalog"
    srcDir="$HOME/haskell/Diakaesten"
else
    catSrc="$curCat"
    srcDir="$HOME/Bilder/Diakaesten"
    catDst="$extCat"
fi
dstDir="$catDst/photos"

dry="--dry-run"
DRY="echo DRY-RUN> "

if isExec
then
    dry=
    DRY=
fi

[[ -d "$srcDir" ]] || die "media source directory not fount: $srcDir"
[[ -d "$catDst" ]] || die "catalog destination directory not fount: $dstDir"
[[ -d "$dstDir" ]] || $DRY mkdir "$dstDir"

trc compute exclude options for media files

excludes=""
for i in $(find "$srcDir" -type f | \
               sed -e 's|^.*[.]|.|'| \
               sort -u | \
               grep -i -v -E '.jpg|.jpeg|.gif|.md|.mp4|.tiff?|.png|.pbm|.pgm|.ppm|.md|.txt' | \
               cat )
do
    trc "files with extension \"$i\" are excluded"
    excludes="$excludes --exclude=*$i"
done

trc copy all media files from $srcDir to $dstDir
trc rsync $dry -av $excludes "$srcDir/" "$dstDir"

# no .-files and no files not used by catalog server
isOrg && \
    rsync $dry -av \
          --exclude='.*' \
          $excludes \
          "$srcDir/" "$dstDir"

srcData="$catSrc/data"
dstData="$catDst/data"

trc copy catalog data from $srcData/ to $dstData

# git repos and configs (.git, .gitignore) are includd
isOrg && \
    rsync $dry -av \
          --exclude='.DS_Store' \
          --exclude='*~' \
          --exclude='dist-newstyle' \
          --exclude='catalog-journal*.json' \
          "$srcData/" "$dstData"


trc copy catalog sources and binaries from $catSrc/ to $catDst

# git repos and configs (.git, .gitignore) are included
( isOrg || isDev ) && \
    rsync $dry -av \
          --exclude='.DS_Store' \
          --exclude='*~' \
          --exclude='data' \
          --exclude='dist-newstyle' \
          --exclude='catalog-journal*.json' \
          "$catSrc/" "$catDst"
