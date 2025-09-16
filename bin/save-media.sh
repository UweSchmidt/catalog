#!/bin/bash

# set -x

# rsync all media files from "Diakaesten" with a media backup

function die() {
    echo $1 1>&2
    exit 1
}

function trc() {
    echo '#' $* 1>&2
}

host=$(hostname | sed -e 's|.local||')
arch=$(arch)

function isTest()        { [[ "yes"   = "yes" ]]; }
function isScheibe()     { [[ "$host" = "scheibe"     ]] || isTest; }
function isSchwarzbuch() { [[ "$host" = "schwarzbuch" ]] || isTest; }

isScheibe || die "media files and catalog data must be copied from scheibe"

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

isScheibe && \
    rsync $dry -av \
          --exclude='.DS_Store' \
          $excludes \
          "$srcDir/" "$dstDir"

srcData="$catSrc/data"
dstData="$catDst/data"

trc copy catalog data from $srcData/ to $dstData

isScheibe && \
    rsync $dry -av \
          --exclude='.*' \
          --exclude='*~' \
          --exclude='dist-newstyle' \
          --exclude='catalog-journal*.json' \
          "$srcData/" "$dstData"

srcBin=$catSrc/bin
dstBin=$catDst/bin

trc copy catalog binaries from $srcBin/ to $dstBin

rsync $dry -av \
      --exclude='.*' \
      --exclude='*~' \
      --exclude='dist-newstyle' \
      --exclude='catalog-journal*.json' \
      "$srcBin/" "$dstBin"

archBin="$dstBin/$arch"

trc save binaries in subdir "$archBin"

[[ -d "$archBin" ]] || $DRY mkdir "$archBin"

for i in servant-polysemy client-polysemy
do
    $DRY cp "$dstBin/$i" "$archBin"
done
