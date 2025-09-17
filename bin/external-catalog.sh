#!/bin/bash

# set -x

# rsync all media files from "Diakaesten" with a media backup

function die() {
    echo $1 1>&2
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
    local ad=$(dirname "$wd/$sp")

    { pushd "$ad"; res=$(pwd -P); popd; } >/dev/null

    # trc "$wd $sp $ad $res"
    trc physDir: result: "$res"
}

orgCat="/Volumes/8TB-SieGeht/home/uwe/Bilder/catalog"
devCat="/Users/uwe/haskell/catalog"
extCat="/Volumes/5TB-Diakasten/home/uwe/Bilder/catalog"

physDir "$0"
curCat=$(dirname "$res")

function isTest() { [[ "yes"   = "yes" ]]; }

function isOrg() { [[ "$curCat" == "$orgCat" ]] || isTest; }
function isDev() { [[ "$curCat" == "$devCat" ]] || isTest; }
function isExt() { [[ "$curCat" == "$extCat" ]] || isTest; }

isOrg || isDev || isExt || die "unknown catalog dir: $curCat"

if isTest
then
    defaultCatSrc="$HOME/haskell/catalog"
    defaultCatDst="$HOME/tmp/catalog"
    defaultSrcDir="$HOME/haskell/Diakaesten"
else
    defaultDisk="5TB-Diakasten"
    defaultSrcDir="$HOME/Bilder/Diakaesten"
    defaultCatDst="/Volumes/$defaultDisk/home/uwe/Bilder/catalog"
fi

dry="--dry-run"
DRY="echo DRY-RUN> "


if [[ "$1" = "-x" ]]
then
    run=1
    shift
    dry=
    DRY=
fi

catSrc="$defaultCatSrc"
catDst="$defaultCatDst"
srcDir="$defaultSrcDir"
dstDir="$catDst/photos"

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

isOrg && \
    rsync $dry -av \
          --exclude='.DS_Store' \
          $excludes \
          "$srcDir/" "$dstDir"

srcData="$catSrc/data"
dstData="$catDst/data"

trc copy catalog data from $srcData/ to $dstData

isOrg && \
    rsync $dry -av \
          --exclude='.*' \
          --exclude='*~' \
          --exclude='dist-newstyle' \
          --exclude='catalog-journal*.json' \
          "$srcData/" "$dstData"

srcBin=$catSrc/bin
dstBin=$catDst/bin

trc copy catalog sources and binaries from $catSrc/ to $catDst

( isOrg || isDev ) && \
    rsync $dry -av \
      --exclude='.*' \
      --exclude='*~' \
      --exclude='data' \
      --exclude='dist-newstyle' \
      --exclude='catalog-journal*.json' \
      "$catSrc/" "$catDst"
